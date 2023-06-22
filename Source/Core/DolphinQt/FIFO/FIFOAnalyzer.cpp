// Copyright 2018 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinQt/FIFO/FIFOAnalyzer.h"

#include <algorithm>

#include <QApplication>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPalette>
#include <QPushButton>
#include <QSplitter>
#include <QTextBrowser>
#include <QTreeWidget>
#include <QTreeWidgetItem>

#include "Common/Assert.h"
#include "Common/MathUtil.h"
#include "Common/Swap.h"
#include "Core/FifoPlayer/FifoPlayer.h"

#include "DolphinQt/QtUtils/NonDefaultQPushButton.h"
#include "DolphinQt/Settings.h"

#include "VideoCommon/BPMemory.h"
#include "VideoCommon/CPMemory.h"
#include "VideoCommon/OpcodeDecoding.h"
#include "VideoCommon/VertexLoaderBase.h"
#include "VideoCommon/VideoCommon.h"
#include "VideoCommon/XFStructs.h"

// Values range from 0 to number of frames - 1
constexpr int FRAME_ROLE = Qt::UserRole;
// Values range from 0 to number of parts - 1
constexpr int PART_START_ROLE = Qt::UserRole + 1;
// Values range from 1 to number of parts
constexpr int PART_END_ROLE = Qt::UserRole + 2;
// Analyze initial viewport and projection matrix
constexpr int LAYER_ROLE = Qt::UserRole + 3;
constexpr int EFBCOPY_ROLE = Qt::UserRole + 4;
constexpr int TYPE_ROLE = Qt::UserRole + 5;
constexpr int OBJECT_START_ROLE = Qt::UserRole + 6;
constexpr int OBJECT_END_ROLE = Qt::UserRole + 6;

constexpr int TYPE_WHOLE = 1;
constexpr int TYPE_FRAME = 2;
constexpr int TYPE_XFBCOPY = 3;
constexpr int TYPE_INHERITED_LAYER = 4;
constexpr int TYPE_LAYER = 5;
constexpr int TYPE_EFBCOPY = 6;
constexpr int TYPE_OBJECT = 7;

constexpr int LINE_LENGTH = 100;

QString primitive_names[] = {QStringLiteral("quads"),      QStringLiteral("Quads_2"),
                             QStringLiteral("tris"),       QStringLiteral("tri-strip"),
                             QStringLiteral("fan"),        QStringLiteral("lines"),
                             QStringLiteral("line-strip"), QStringLiteral("points")};

namespace
{
class SimulateCallback : public OpcodeDecoder::Callback
{
public:
  explicit SimulateCallback(CPState cpmem, XFMemory* xf, BPMemory* bp)
      : m_cpmem(cpmem), xfmem(xf), bpmem(bp), viewport_set(false), projection_set(false),
        scissor_set(false), scissor_offset_set(false), efb_copied(false)
  {
  }

  OPCODE_CALLBACK(void OnCP(u8 command, u32 value)) {}

  OPCODE_CALLBACK(void OnXF(u16 baseAddress, u8 transferSize, const u8* data))
  {
    // do not allow writes past registers
    if (baseAddress + transferSize > XFMEM_REGISTERS_END)
    {
      if (baseAddress >= XFMEM_REGISTERS_END)
        transferSize = 0;
      else
        transferSize = XFMEM_REGISTERS_END - baseAddress;
    }
    u32 address = baseAddress;
    u32* xfm = (u32*)xfmem;
    while (transferSize > 0 && address < XFMEM_REGISTERS_END)
    {
      u32 newValue = Common::swap32(data);
      xfm[address] = newValue;
      if (XFMEM_SETPROJECTION <= address && address <= XFMEM_SETPROJECTION + 6)
        projection_set = true;
      else if (XFMEM_SETVIEWPORT <= address && address <= XFMEM_SETVIEWPORT + 5)
        viewport_set = true;

      address++;
      transferSize--;
    }
  }

  OPCODE_CALLBACK(void OnBP(u8 cmd, u32 value))
  {
    if (cmd == BPMEM_SCISSORTL || cmd == BPMEM_SCISSORBR)
      scissor_set = true;
    else if (cmd == BPMEM_SCISSOROFFSET)
      scissor_offset_set = true;
    else if (cmd == BPMEM_TRIGGER_EFB_COPY)
      efb_copied = true;

    int oldval = ((u32*)&bpmem)[cmd];
    int newval = (oldval & ~bpmem->bpMask) | (value & bpmem->bpMask);

    ((u32*)bpmem)[cmd] = newval;

    // Reset the mask register if we're not trying to set it ourselves.
    if (cmd != BPMEM_BP_MASK)
      bpmem->bpMask = 0xFFFFFF;
  }

  OPCODE_CALLBACK(void OnIndexedLoad(CPArray array, u32 index, u16 address, u8 size)) {}

  OPCODE_CALLBACK(void OnPrimitiveCommand(OpcodeDecoder::Primitive primitive, u8 vat,
                                          u32 vertex_size, u16 num_vertices, const u8* vertex_data))
  {
    int prim = (int)primitive;

    // We have something different now, so merge the previous similar things
    if (nop_count || prim != prev_prim)
    {
      if (text.length() - broken_length > LINE_LENGTH)
      {
        broken_length = text.length();
        text += QStringLiteral(",\n");
      }
      else if (!text.isEmpty())
      {
        text += QStringLiteral(", ");
      }
      if (drawcall_count == 1)
        text += prev_desc;
      else if (drawcall_count > 1)
        text += QStringLiteral("%1 %2 in %3 %4")
                    .arg(total_prim_count)
                    .arg(QString::fromUtf8(prim_in_calls[prev_prim]))
                    .arg(drawcall_count)
                    .arg(QString::fromUtf8(prim_calls[prev_prim]));
      if (nop_count)
      {
        if (drawcall_count > 0)
          text += QStringLiteral(", ");
        text += QStringLiteral("%1xNOP").arg(nop_count);
      }
      prev_desc.clear();
      prev_prim = 0;
      drawcall_count = 0;
      nop_count = 0;
    }

    drawcall_count++;
    prev_prim = prim;
    prev_desc.clear();
    int prim_count = 0;
    int count = num_vertices;
    switch (prim)
    {
    case 0:
      prim_count = count / 4;
      if (count == 4)
        prev_desc = QStringLiteral("Quad (1 quad)");
      else
        prev_desc = QStringLiteral("%1 quads").arg(prim_count);
      break;
    case 1:
      prim_count = count / 4;
      if (count == 4)
        prev_desc = QStringLiteral("Quad (1 quad2)");
      else
        prev_desc = QStringLiteral("%1 quad2s").arg(prim_count);
      break;
    case 2:
      prim_count = count / 3;
      prev_desc = QStringLiteral("%1 tris").arg(prim_count);
      break;
    case 3:
      prim_count = count - 2;
      if (count == 4)
        prev_desc = QStringLiteral("Quad (2 tri-strip)");
      else if (count == 0)
        prev_desc = QStringLiteral("0 tri-strip");
      else
        prev_desc = QStringLiteral("%1 tri-strip").arg(prim_count);
      break;
    case 4:
      prim_count = count - 2;
      if (count == 4)
        prev_desc = QStringLiteral("Quad (2 fan)");
      else if (count == 0)
        prev_desc = QStringLiteral("0 fan");
      else
        prev_desc = QStringLiteral("%1 fan").arg(prim_count);
      break;
    case 5:
      prim_count = count / 2;
      prev_desc = QStringLiteral("%1 lines").arg(prim_count);
      break;
    case 6:
      prim_count = count - 1;
      if (count == 0)
        prev_desc = QStringLiteral("0 linestrip");
      else
        prev_desc = QStringLiteral("%1 linestrip").arg(prim_count);
      break;
    case 7:
      prim_count = count;
      prev_desc = QStringLiteral("%1 points").arg(prim_count);
      break;
    }
    if (!count)
      prim_count = 0;
    total_prim_count += prim_count;
  }

  void finish_desc()
  {
    if (nop_count || -1 != prev_prim)
    {
      if (text.length() - broken_length > LINE_LENGTH)
      {
        broken_length = text.length();
        text += QStringLiteral(",\n");
      }
      else if (!text.isEmpty())
      {
        text += QStringLiteral(", ");
      }
      if (drawcall_count == 1)
        text += prev_desc;
      else if (drawcall_count > 1)
        text += QStringLiteral("%1 %2 in %3 %4")
                    .arg(total_prim_count)
                    .arg(QString::fromUtf8(prim_in_calls[prev_prim]))
                    .arg(drawcall_count)
                    .arg(QString::fromUtf8(prim_calls[prev_prim]));
      if (nop_count)
      {
        if (drawcall_count > 0)
          text += QStringLiteral(", ");
        text += QStringLiteral("%1xNOP").arg(nop_count);
      }
    }
  }

  OPCODE_CALLBACK(void OnDisplayList(u32 address, u32 size))
  {
    // todo: check how to handle display lists in Fifo Player
  }

  OPCODE_CALLBACK(void OnNop(u32 count)) { nop_count += count; }

  OPCODE_CALLBACK(void OnUnknown(u8 opcode, const u8* data)) {}

  OPCODE_CALLBACK(void OnCommand(const u8* data, u32 size)) {}

  OPCODE_CALLBACK(CPState& GetCPState()) { return m_cpmem; }

  OPCODE_CALLBACK(u32 GetVertexSize(u8 vat))
  {
    return VertexLoaderBase::GetVertexSize(GetCPState().vtx_desc, GetCPState().vtx_attr[vat]);
  }

  QString text;
  CPState m_cpmem;
  XFMemory* xfmem;
  BPMemory* bpmem;
  bool projection_set, viewport_set, scissor_set, scissor_offset_set, efb_copied;

  const char* prim_in_calls[8] = {"quads", "quad2s", "tris",  "tris",
                                  "tris",  "lines",  "lines", "points"};
  const char* prim_calls[8] = {"calls", "calls", "calls",  "strips",
                               "fans",  "calls", "strips", "calls"};
  // Keep track of previous similar draw calls so we can merge them
  QString prev_desc;
  int prev_prim = -1;
  int drawcall_count = 0;
  int total_prim_count = 0;
  int nop_count = 0;
  int broken_length = 0;
};
}  // namespace

FIFOAnalyzer::FIFOAnalyzer()
{
  CreateWidgets();
  ConnectWidgets();

  UpdateTree();

  auto& settings = Settings::GetQSettings();

  m_object_splitter->restoreState(
      settings.value(QStringLiteral("fifoanalyzer/objectsplitter")).toByteArray());
  m_search_splitter->restoreState(
      settings.value(QStringLiteral("fifoanalyzer/searchsplitter")).toByteArray());

  m_detail_list->setFont(Settings::Instance().GetDebugFont());
  m_entry_detail_browser->setFont(Settings::Instance().GetDebugFont());

  connect(&Settings::Instance(), &Settings::DebugFontChanged, this, [this] {
    m_detail_list->setFont(Settings::Instance().GetDebugFont());
    m_entry_detail_browser->setFont(Settings::Instance().GetDebugFont());
  });
}

FIFOAnalyzer::~FIFOAnalyzer()
{
  auto& settings = Settings::GetQSettings();

  settings.setValue(QStringLiteral("fifoanalyzer/objectsplitter"), m_object_splitter->saveState());
  settings.setValue(QStringLiteral("fifoanalyzer/searchsplitter"), m_search_splitter->saveState());
}

void FIFOAnalyzer::CreateWidgets()
{
  m_tree_widget = new QTreeWidget;
  m_detail_list = new QListWidget;
  m_entry_detail_browser = new QTextBrowser;

  // m_tree_widget->setObjectName("m_tree_widget");
  // m_detail_list->setObjectName("m_detail_list");
  // m_entry_detail_browser->setObjectName("m_entry_detail_browser");
  // setStyleSheet(QStringLiteral("AnyQtWidget#m_tree_widget {}"));

  m_object_splitter = new QSplitter(Qt::Horizontal);

  m_object_splitter->addWidget(m_tree_widget);
  m_object_splitter->addWidget(m_detail_list);

  m_tree_widget->header()->hide();
  m_tree_widget->setSelectionMode(QAbstractItemView::SelectionMode::ContiguousSelection);

  m_search_box = new QGroupBox(tr("Search Current Object"));
  m_search_edit = new QLineEdit;
  m_search_new = new NonDefaultQPushButton(tr("Search"));
  m_search_next = new NonDefaultQPushButton(tr("Next Match"));
  m_search_previous = new NonDefaultQPushButton(tr("Previous Match"));
  m_search_label = new QLabel;

  m_search_next->setEnabled(false);
  m_search_previous->setEnabled(false);

  auto* box_layout = new QHBoxLayout;

  box_layout->addWidget(m_search_edit);
  box_layout->addWidget(m_search_new);
  box_layout->addWidget(m_search_next);
  box_layout->addWidget(m_search_previous);
  box_layout->addWidget(m_search_label);

  m_search_box->setLayout(box_layout);

  m_search_box->setMaximumHeight(m_search_box->minimumSizeHint().height());

  m_search_splitter = new QSplitter(Qt::Vertical);

  m_search_splitter->addWidget(m_object_splitter);
  m_search_splitter->addWidget(m_entry_detail_browser);
  m_search_splitter->addWidget(m_search_box);

  auto* layout = new QHBoxLayout;
  layout->addWidget(m_search_splitter);

  setLayout(layout);
}

void FIFOAnalyzer::ConnectWidgets()
{
  connect(m_tree_widget, &QTreeWidget::itemSelectionChanged, this, &FIFOAnalyzer::UpdateDetails);
  connect(m_detail_list, &QListWidget::itemSelectionChanged, this,
          &FIFOAnalyzer::UpdateDescription);

  connect(m_search_edit, &QLineEdit::returnPressed, this, &FIFOAnalyzer::BeginSearch);
  connect(m_search_new, &QPushButton::clicked, this, &FIFOAnalyzer::BeginSearch);
  connect(m_search_next, &QPushButton::clicked, this, &FIFOAnalyzer::FindNext);
  connect(m_search_previous, &QPushButton::clicked, this, &FIFOAnalyzer::FindPrevious);
}

void FIFOAnalyzer::Update()
{
  UpdateTree();
  UpdateDetails();
  UpdateDescription();
}

QString FIFOAnalyzer::DescribeViewport(Viewport* viewport)
{
  float x = viewport->xOrig - viewport->wd - m_bpmem->scissorOffset.x * 2;
  float y = viewport->yOrig + viewport->ht - m_bpmem->scissorOffset.y * 2;

  float width = 2.0f * viewport->wd;
  float height = -2.0f * viewport->ht;
  float min_depth = (viewport->farZ - viewport->zRange) / 16777216.0f;
  float max_depth = viewport->farZ / 16777216.0f;
  if (width < 0.f)
  {
    // x += width;
    // width *= -1;
  }
  if (height < 0.f)
  {
    // y += height;
    // height *= -1;
  }

  return QStringLiteral("VP (%1, %2) %3 x %4, z %5 to %6")
      .arg(x)
      .arg(y)
      .arg(width)
      .arg(height)
      .arg(min_depth)
      .arg(max_depth);
}

QString FIFOAnalyzer::DescribeScissor()
{
  const int xoff = m_bpmem->scissorOffset.x * 2;
  const int yoff = m_bpmem->scissorOffset.y * 2;

  MathUtil::Rectangle<int> r(m_bpmem->scissorTL.x - xoff, m_bpmem->scissorTL.y - yoff,
                             m_bpmem->scissorBR.x - xoff + 1, m_bpmem->scissorBR.y - yoff + 1);

  return QStringLiteral("Scissor (%1, %2) %3 x %4")
      .arg(r.left)
      .arg(r.top)
      .arg(r.GetWidth())
      .arg(r.GetHeight());
}

static bool AlmostEqual(float a, float b)
{
  constexpr const float epsilon = 0.001f;
  return fabs(a - b) < epsilon;
}

static bool AlmostEqual(MathUtil::Rectangle<float> r, MathUtil::Rectangle<float> r2)
{
  return AlmostEqual(r.left, r2.left) && AlmostEqual(r.right, r2.right) &&
         AlmostEqual(r.top, r2.top) && AlmostEqual(r.bottom, r2.bottom);
}

static bool AlmostEqual(MathUtil::Rectangle<float> r, MathUtil::Rectangle<float> r2,
                        MathUtil::Rectangle<float> r3)
{
  return AlmostEqual(r, r2) && AlmostEqual(r2, r3);
}

QString FIFOAnalyzer::DescribeLayer(bool set_viewport, bool set_scissor, bool set_projection)
{
  QString result;
  const int xoff = m_bpmem->scissorOffset.x * 2;
  const int yoff = m_bpmem->scissorOffset.y * 2;
  MathUtil::Rectangle<float> r_scissor(m_bpmem->scissorTL.x - xoff, m_bpmem->scissorTL.y - yoff,
                                       m_bpmem->scissorBR.x - xoff + 1,
                                       m_bpmem->scissorBR.y - yoff + 1);
  if (!set_scissor)
  {
    r_scissor.left = -1;
    r_scissor.right = -1;
    r_scissor.top = -1;
    r_scissor.bottom = -1;
  }

  float x = m_xfmem->viewport.xOrig - m_xfmem->viewport.wd - xoff;
  float y = m_xfmem->viewport.yOrig + m_xfmem->viewport.ht - yoff;

  float width = 2.0f * m_xfmem->viewport.wd;
  float height = -2.0f * m_xfmem->viewport.ht;
  if (width < 0.f)
  {
    x += width;
    width *= -1;
  }
  if (height < 0.f)
  {
    y += height;
    height *= -1;
  }
  MathUtil::Rectangle<float> r_viewport(x, y, x + width, y + height);
  float min_depth = (m_xfmem->viewport.farZ - m_xfmem->viewport.zRange) / 16777216.0f;
  float max_depth = m_xfmem->viewport.farZ / 16777216.0f;
  if (!set_viewport)
  {
    r_viewport.left = -2;
    r_viewport.right = -2;
    r_viewport.top = -2;
    r_viewport.bottom = -2;
    min_depth = 0;
    max_depth = 1;
  }

  if (set_projection && m_xfmem->projection.type == ProjectionType::Orthographic)
  {
    width = 2 / m_xfmem->projection.rawProjection[0];
    height = -2 / m_xfmem->projection.rawProjection[2];
    x = (-m_xfmem->projection.rawProjection[1] - 1) * width / 2;
    y = (m_xfmem->projection.rawProjection[3] - 1) * height / 2;
  }
  else
  {
    x = -3;
    y = -3;
    width = -3;
    height = -3;
  }
  MathUtil::Rectangle<float> r_projection(x, y, x + width, y + height);

  // Viewport
  if (AlmostEqual(r_viewport, r_scissor, r_projection))
  {
    result = QStringLiteral("VP+Scissor+Proj 2D");
    if (r_viewport.left != 0 || r_viewport.top != 0)
      result += QStringLiteral(" (%1, %2)").arg(r_viewport.left).arg(r_viewport.top);
    result += QStringLiteral(" %1x%2").arg(r_viewport.GetWidth()).arg(r_viewport.GetHeight());
    set_projection = false;
    set_scissor = false;
  }
  else if (AlmostEqual(r_viewport, r_scissor))
  {
    result = QStringLiteral("VP+Scissor");
    if (r_viewport.left != 0 || r_viewport.top != 0)
      result += QStringLiteral(" (%1, %2)").arg(r_viewport.left).arg(r_viewport.top);
    result += QStringLiteral(" %1x%2").arg(r_viewport.GetWidth()).arg(r_viewport.GetHeight());
    set_scissor = false;
  }
  else if (AlmostEqual(r_viewport, r_projection))
  {
    result = QStringLiteral("VP+Proj 2D");
    if (r_viewport.left != 0 || r_viewport.top != 0)
      result += QStringLiteral(" (%1, %2)").arg(r_viewport.left).arg(r_viewport.top);
    result += QStringLiteral(" %1x%2").arg(r_viewport.GetWidth()).arg(r_viewport.GetHeight());
    set_projection = false;
  }
  else if (set_viewport)
  {
    result = QStringLiteral("VP");
    if (r_viewport.left != 0 || r_viewport.top != 0)
      result += QStringLiteral(" (%1, %2)").arg(r_viewport.left).arg(r_viewport.top);
    result += QStringLiteral(" %1x%2").arg(r_viewport.GetWidth()).arg(r_viewport.GetHeight());
  }

  // Scissor
  if (set_scissor)
  {
    if (set_viewport)
      result += QStringLiteral(" ");
    if (set_scissor && set_projection && AlmostEqual(r_scissor, r_projection))
    {
      result += QStringLiteral("Scissor+Proj 2D");
      set_projection = false;
    }
    else if (set_scissor)
    {
      result += QStringLiteral("Scissor");
    }
    if (r_scissor.left != 0 || r_scissor.top != 0)
      result += QStringLiteral(" (%1, %2)").arg(r_scissor.left).arg(r_scissor.top);
    result += QStringLiteral(" %1x%2").arg(r_scissor.GetWidth()).arg(r_scissor.GetHeight());
  }

  // Projection
  if (set_projection)
  {
    if (set_viewport || set_scissor)
      result += QStringLiteral(" ");
    if (m_xfmem->projection.type == ProjectionType::Orthographic)
    {
      result += QStringLiteral("Proj 2D");
      if (r_projection.left != 0 || r_projection.top != 0)
        result += QStringLiteral(" (%1, %2)").arg(r_projection.left).arg(r_projection.top);
      result += QStringLiteral(" %1x%2").arg(r_projection.GetWidth()).arg(r_projection.GetHeight());
    }
    else
    {
      float a = m_xfmem->projection.rawProjection[4];
      float b = m_xfmem->projection.rawProjection[5];
      float n = -b / (1 - a);
      float f = b / a;
      float t = 1 / m_xfmem->projection.rawProjection[0];
      float hfov = atan(t) * 2;
      float vfov = atan(1 / m_xfmem->projection.rawProjection[2]) * 2;
      float aspect = tan(hfov / 2) / tan(vfov / 2);
      result += QStringLiteral("FOV %1\302\260 x %2\302\260, AR 16:%3, near %4 far %5")
                    .arg(hfov * 360 / float(MathUtil::TAU))
                    .arg(vfov * 360 / float(MathUtil::TAU))
                    .arg(16 / aspect)
                    .arg(n)
                    .arg(f);
    }
  }
  if (min_depth != 0 || max_depth != 1)
  {
    result += QStringLiteral(", z %1 to %2").arg(min_depth).arg(max_depth);
  }
  return result;
}

QString FIFOAnalyzer::DescribeEFBCopy(QString* resolution)
{
  u32 destAddr = m_bpmem->copyTexDest << 5;
  u32 destStride = m_bpmem->copyMipMapStrideChannels << 5;

  MathUtil::Rectangle<int> srcRect;
  srcRect.left = static_cast<int>(m_bpmem->copyTexSrcXY.x);
  srcRect.top = static_cast<int>(m_bpmem->copyTexSrcXY.y);

  // Here Width+1 like Height, otherwise some textures are corrupted already since the native
  // resolution.
  srcRect.right = static_cast<int>(m_bpmem->copyTexSrcXY.x + m_bpmem->copyTexSrcWH.x + 1);
  srcRect.bottom = static_cast<int>(m_bpmem->copyTexSrcXY.y + m_bpmem->copyTexSrcWH.y + 1);
  // int copy_width = srcRect.GetWidth();
  // int copy_height = srcRect.GetHeight();
  if (srcRect.right > s32(EFB_WIDTH) || srcRect.bottom > s32(EFB_HEIGHT))
  {
    // WARN_LOG(VIDEO, "Oversized EFB copy: %dx%d (offset %d,%d stride %u)", copy_width,
    // copy_height,
    //         srcRect.left, srcRect.top, destStride);

    // Adjust the copy size to fit within the EFB. So that we don't end up with a stretched image,
    // instead of clamping the source rectangle, we reduce it by the over-sized amount.
    // if (copy_width > s32(EFB_WIDTH))
    //{
    //  srcRect.right -= copy_width - EFB_WIDTH;
    //  copy_width = EFB_WIDTH;
    //}
    // if (copy_height > s32(EFB_HEIGHT))
    //{
    //  srcRect.bottom -= copy_height - EFB_HEIGHT;
    //  copy_height = EFB_HEIGHT;
    //}
  }

  bool is_depth_copy = m_bpmem->zcontrol.pixel_format == PixelFormat::Z24;
  QString result;
  if (is_depth_copy)
    result = QStringLiteral("Depth ");
  // Check if we are to copy from the EFB or draw to the XFB
  const UPE_Copy PE_copy = m_bpmem->triggerEFBCopy;
  if (PE_copy.copy_to_xfb == 0)
  {
    // analyzer_bpmem.zcontrol.pixel_format to PEControl::Z24 is when the game wants to copy from
    // ZBuffer (Zbuffer uses 24-bit Format)
    // static constexpr CopyFilterCoefficients::Values filter_coefficients = {
    //    {0, 0, 21, 22, 21, 0, 0}};
    // g_texture_cache->CopyRenderTargetToTexture(
    //    destAddr, PE_copy.tp_realFormat(), copy_width, copy_height, destStride, is_depth_copy,
    //    srcRect, !!PE_copy.intensity_fmt, !!PE_copy.half_scale, 1.0f, 1.0f,
    //    analyzer_bpmem.triggerEFBCopy.clamp_top, analyzer_bpmem.triggerEFBCopy.clamp_bottom,
    //    filter_coefficients);
    result += QStringLiteral("Copy to Tex[%1 %2]").arg(destAddr, 0, 16).arg(destStride);
  }
  else
  {
    float yScale;
    if (PE_copy.scale_invert)
      yScale = 256.0f / static_cast<float>(m_bpmem->dispcopyyscale);
    else
      yScale = static_cast<float>(m_bpmem->dispcopyyscale) / 256.0f;

    float num_xfb_lines = 1.0f + m_bpmem->copyTexSrcWH.y * yScale;

    u32 height = static_cast<u32>(num_xfb_lines);

    // DEBUG_LOG(VIDEO,
    //          "RenderToXFB: destAddr: %08x | srcRect {%d %d %d %d} | fbWidth: %u | "
    //          "fbStride: %u | fbHeight: %u | yScale: %f",
    //          destAddr, srcRect.left, srcRect.top, srcRect.right, srcRect.bottom,
    //          m_bpmem->copyTexSrcWH.x + 1, destStride, height, yScale);

    // g_texture_cache->CopyRenderTargetToTexture(
    //    destAddr, EFBCopyFormat::XFB, copy_width, height, destStride, is_depth_copy, srcRect,
    //    false, false, yScale, s_gammaLUT[PE_copy.gamma], m_bpmem->triggerEFBCopy.clamp_top,
    //    m_bpmem->triggerEFBCopy.clamp_bottom, m_bpmem->copyfilter.GetCoefficients());

    result +=
        QStringLiteral("Copy to XFB[%1 %2x%3]").arg(destAddr, 0, 16).arg(destStride).arg(height);
  }
  QString res;
  if ((!AlmostEqual(srcRect.left, 0)) || (!AlmostEqual(srcRect.top, 0)))
    res += QStringLiteral(" (%1, %2)").arg(srcRect.left).arg(srcRect.top);
  res += QStringLiteral(" %1x%2").arg(srcRect.GetWidth()).arg(srcRect.GetHeight());
  result += res;
  if (resolution)
    *resolution = res;

  if (PE_copy.intensity_fmt)
    result += QStringLiteral(", Intensity");

  // Clear the rectangular region after copying it.
  if (PE_copy.clear)
    result += QStringLiteral(", Clear");

  return result;
}

QString FIFOAnalyzer::DescribeProjection(Projection* proj)
{
  if (proj->type == ProjectionType::Orthographic)
  {
    float w = 2 / proj->rawProjection[0];
    float h = -2 / proj->rawProjection[2];
    return QStringLiteral("Proj 2D %1 x %2").arg(w).arg(h);
  }
  else
  {
    float a = proj->rawProjection[4];
    float b = proj->rawProjection[5];
    float n = -b / (1 - a);
    float f = b / a;
    float t = 1 / proj->rawProjection[0];
    float hfov = atan(t) * 2;
    float vfov = atan(1 / proj->rawProjection[2]) * 2;
    float aspect = tan(hfov / 2) / tan(vfov / 2);
    return QStringLiteral("Proj FOV %1 x %2 deg, AR 16:%3, z %4 to %5")
        .arg(hfov * 360 / float(MathUtil::TAU))
        .arg(vfov * 360 / float(MathUtil::TAU))
        .arg(16 / aspect)
        .arg(n)
        .arg(f);
  }
}

void FIFOAnalyzer::UpdateTree()
{
  m_tree_widget->clear();

  if (!FifoPlayer::GetInstance().IsPlaying())
  {
    auto* recording_item = new QTreeWidgetItem({tr("No recording loaded.")});
    recording_item->setData(0, TYPE_ROLE, TYPE_WHOLE);
    m_tree_widget->addTopLevelItem(recording_item);
    return;
  }

  auto* recording_item = new QTreeWidgetItem({tr("Recording")});
  QPalette blue_palette, green_palette, red_palette;
  QColor color;
  // projection/viewport changes will be blue
  color.setRgb(0, 80, 255);
  blue_palette.setColor(QPalette::Text, color);
  // scissor changes without a projection/viewport change will be green
  color.setRgb(200, 0, 0);
  red_palette.setColor(QPalette::Text, color);
  // all kinds of EFB copies (EFB copies/XFB copies/frames) will be red
  color.setRgb(10, 180, 0);
  green_palette.setColor(QPalette::Text, color);

  m_tree_widget->addTopLevelItem(recording_item);
  recording_item->setData(0, TYPE_ROLE, TYPE_WHOLE);

  auto* file = FifoPlayer::GetInstance().GetFile();

  const u32 frame_count = file->GetFrameCount();

  // keep track of the registers and which relevant ones have been modified
  {
    if (!m_xfmem)
      m_xfmem = std::make_unique<XFMemory>();
    if (!m_bpmem)
      m_bpmem = std::make_unique<BPMemory>();
    u32* p = file->GetXFMem();
    memcpy(m_xfmem.get(), p, 0x1000 * sizeof(u32));
    p = file->GetXFRegs();
    memcpy(&m_xfmem->error, p, 0x58 * sizeof(u32));
    p = file->GetBPMem();
    memcpy(m_bpmem.get(), p, sizeof(BPMemory));
  }

  bool projection_set = false;
  bool viewport_set = false;
  bool scissor_set = false;
  bool scissor_offset_set = false;
  bool efb_copied = false;

  for (u32 frame = 0; frame < frame_count; frame++)
  {
    auto* frame_item = new QTreeWidgetItem({tr("Frame %1").arg(frame)});
    frame_item->setData(0, TYPE_ROLE, TYPE_FRAME);
    frame_item->setData(0, FRAME_ROLE, frame);
    frame_item->setData(0, Qt::ForegroundRole, red_palette.color(QPalette::Text));

    recording_item->addChild(frame_item);

    const AnalyzedFrameInfo& frame_info = FifoPlayer::GetInstance().GetAnalyzedFrameInfo(frame);
    ASSERT(frame_info.parts.size() != 0);

    Common::EnumMap<u32, FramePartType::EFBCopy> part_counts;
    u32 part_start = 0;
    int layer = 0;
    int efbcopy_count = 0;

    for (u32 part_nr = 0; part_nr < frame_info.parts.size(); part_nr++)
    {
      // add projection and viewport inherited from previous frame as layer 0
      if (part_nr == 0)
      {
        QString s = QStringLiteral("inherited: %1").arg(DescribeLayer(true, true, true));
        auto* layer_item = new QTreeWidgetItem({s});
        layer_item->setData(0, FRAME_ROLE, frame);
        layer_item->setData(0, LAYER_ROLE, layer);
        layer_item->setData(0, Qt::ForegroundRole, blue_palette.color(QPalette::Text));
        frame_item->addChild(layer_item);
        layer++;
      }
      const auto& part = frame_info.parts[part_nr];

      const u32 part_type_nr = part_counts[part.m_type];
      part_counts[part.m_type]++;

      QTreeWidgetItem* object_item = nullptr;
      if (part.m_type == FramePartType::PrimitiveData)
      {
        QString obj_desc;
        CheckObject(frame, part_start, part_nr, m_xfmem.get(), m_bpmem.get(), &projection_set,
                    &viewport_set, &scissor_set, &scissor_offset_set, &efb_copied, &obj_desc);
        QString adjectives = GetAdjectives();
        object_item = new QTreeWidgetItem(
            {tr("Object %1:\t%2  \t%3").arg(part_type_nr).arg(obj_desc).arg(adjectives)});
        object_item->setData(0, TYPE_ROLE, TYPE_OBJECT);
        object_item->setData(0, OBJECT_START_ROLE, part_type_nr);
        object_item->setData(0, OBJECT_END_ROLE, part_type_nr);
        if (scissor_offset_set)
        {
          scissor_set = true;
          viewport_set = true;
        }
        if (projection_set || viewport_set || scissor_set)
        {
          QString s = QStringLiteral("%1: %2").arg(layer).arg(
              DescribeLayer(viewport_set, scissor_set, projection_set));
          auto* layer_item = new QTreeWidgetItem({s});
          layer_item->setData(0, TYPE_ROLE, TYPE_LAYER);
          layer_item->setData(0, FRAME_ROLE, frame);
          layer_item->setData(0, LAYER_ROLE, layer);
          if (viewport_set || projection_set)
            layer_item->setData(0, Qt::ForegroundRole, blue_palette.color(QPalette::Text));
          else
            layer_item->setData(0, Qt::ForegroundRole, green_palette.color(QPalette::Text));
          QTreeWidgetItem* parent = frame_item;
          FoldLayer(parent);
          parent->addChild(layer_item);
          layer++;
        }
      }
      else if (part.m_type == FramePartType::EFBCopy)
      {
        QString obj_desc;
        CheckObject(frame, part_start, part_nr, m_xfmem.get(), m_bpmem.get(), &projection_set,
                    &viewport_set, &scissor_set, &scissor_offset_set, &efb_copied, &obj_desc);
        QString resolution;
        QString efb_copy = DescribeEFBCopy(&resolution);
        QString s = QStringLiteral("EFB Copy %1: %2").arg(efbcopy_count).arg(efb_copy);
        auto* efbcopy_item = new QTreeWidgetItem({s});
        efbcopy_item->setData(0, TYPE_ROLE, TYPE_EFBCOPY);
        efbcopy_item->setData(0, FRAME_ROLE, frame);
        efbcopy_item->setData(0, EFBCOPY_ROLE, efbcopy_count);
        efbcopy_item->setData(0, Qt::ForegroundRole, red_palette.color(QPalette::Text));
        QTreeWidgetItem* parent = frame_item;
        if (part_nr == frame_info.parts.size() - 1)
        {
          efbcopy_item->setData(0, PART_START_ROLE, part_start);
          efbcopy_item->setData(0, PART_END_ROLE, part_nr);
          efbcopy_item->setText(0, QStringLiteral("XFB Copy: %1").arg(efb_copy));
          efbcopy_item->setData(0, TYPE_ROLE, TYPE_XFBCOPY);
          frame_item->setText(0, QStringLiteral("Frame %1: %2").arg(frame).arg(resolution));
        }
        else
        {
          FoldLayer(parent);
          int first = parent->childCount() - 1;
          while (first > 0)
          {
            QTreeWidgetItem* item = parent->child(first);
            if (!item->data(0, EFBCOPY_ROLE).isNull())
              break;
            first--;
          }
          first++;
          while (first < parent->childCount())
          {
            efbcopy_item->addChild(parent->takeChild(first));
          }
        }
        parent->addChild(efbcopy_item);
        // if we don't clear the screen after the EFB Copy, we should still be able to see what's
        // inside it so reflect that in our tree too
        // efbcopy_item->setExpanded((!(m_bpmem->triggerEFBCopy.clear)) ||
        //                        m_bpmem->triggerEFBCopy.copy_to_xfb);
        efbcopy_item->setExpanded(false);
        efbcopy_count++;
        part_start = part_nr + 1;
        object_item = nullptr;
      }
      // We don't create dedicated labels for FramePartType::Command;
      // those are grouped with the primitive

      if (object_item != nullptr)
      {
        frame_item->addChild(object_item);

        object_item->setData(0, FRAME_ROLE, frame);
        object_item->setData(0, PART_START_ROLE, part_start);
        object_item->setData(0, PART_END_ROLE, part_nr);

        part_start = part_nr + 1;
      }
#if 0
      if (part_nr == frame_info.parts.size() - 1)
      {
        QTreeWidgetItem* parent = frame_item;
        int first = parent->childCount() - 1;
        QTreeWidgetItem* first_item = nullptr;
        while (first >= 0)
        {
          first_item = parent->child(first);
          if (!first_item->data(0, EFBCOPY_ROLE).isNull())
            break;
          if (!first_item->data(0, LAYER_ROLE).isNull())
            break;
          first--;
        }
        first++;
        if (first_item && first_item->data(0, EFBCOPY_ROLE).isNull())
        {
          while (first < parent->childCount())
          {
            first_item->addChild(parent->takeChild(first));
          }
          // everything inside a layer can still be seen
          // so reflect that in our tree too
          first_item->setExpanded(true);
        }
      }
#endif
    }
    recording_item->setExpanded(true);

    // We shouldn't end on a Command (it should end with an EFB copy)
    ASSERT(part_start == frame_info.parts.size());
    // The counts we computed should match the frame's counts
    ASSERT(std::equal(frame_info.part_type_counts.begin(), frame_info.part_type_counts.end(),
                      part_counts.begin()));
  }
}

void FIFOAnalyzer::FoldLayer(QTreeWidgetItem* parent)
{
  int first = parent->childCount() - 1;
  QTreeWidgetItem* first_item = nullptr;
  while (first >= 0)
  {
    first_item = parent->child(first);
    if (!first_item->data(0, EFBCOPY_ROLE).isNull())
      break;
    if (!first_item->data(0, LAYER_ROLE).isNull())
      break;
    first--;
  }
  first++;
  if (first_item && first_item->data(0, EFBCOPY_ROLE).isNull())
  {
    while (first < parent->childCount())
    {
      first_item->addChild(parent->takeChild(first));
    }
    // everything inside a layer can still be seen
    // so reflect that in our tree too
    first_item->setExpanded(true);
  }
}

QString FIFOAnalyzer::GetAdjectives()
{
  std::string a;
  if (m_bpmem->genMode.zfreeze)
    a += "zfreeze ";
  if (m_bpmem->genMode.flat_shading)
    a += "flat-shading? ";
  if (((m_bpmem->zmode.testenable) && (m_bpmem->zmode.func == CompareMode::Never)) ||
      (m_bpmem->genMode.cullmode == CullMode::All))
    a += "not-drawn ";
  else if ((!m_bpmem->zmode.testenable) || (m_bpmem->zmode.func == CompareMode::Always))
    a += "always-on-top ";
  if (m_bpmem->genMode.cullmode == CullMode::None)
    a += "double-sided ";
  else if (m_bpmem->genMode.cullmode == CullMode::Front)
    a += "backface ";
  if (m_bpmem->fog.c_proj_fsel.fsel != FogType::Off)
    a += "fogged ";
  if (m_bpmem->blendmode.logicopenable)
    a += "logic-op ";
  bool alpha_blended =
      (m_bpmem->blendmode.blendenable && m_bpmem->blendmode.srcfactor == SrcBlendFactor::SrcAlpha &&
       m_bpmem->blendmode.dstfactor == DstBlendFactor::InvSrcAlpha);
  bool additive =
      (m_bpmem->blendmode.blendenable && m_bpmem->blendmode.srcfactor == SrcBlendFactor::SrcAlpha &&
       m_bpmem->blendmode.dstfactor == DstBlendFactor::One);
  bool full_additive =
      (m_bpmem->blendmode.blendenable && m_bpmem->blendmode.srcfactor == SrcBlendFactor::One &&
       m_bpmem->blendmode.dstfactor == DstBlendFactor::One);
  if (alpha_blended)
    a += "alpha-blended ";
  else if (full_additive)
    a += "100%-additive ";
  else if (additive)
    a += "additive ";

  return QString::fromStdString(a);
}

int ItemsFirstObject(QTreeWidgetItem* item, bool allow_siblings = false)
{
  // if it's the entire frame or sequence, start at the beginning
  if (!item->data(0, TYPE_ROLE).isNull())
  {
    int type = item->data(0, TYPE_ROLE).toInt();
    if (type == TYPE_FRAME || type == TYPE_XFBCOPY || type == TYPE_WHOLE)
      return 0;
  }
  // if it's an object, problem solved
  if (!item->data(0, OBJECT_START_ROLE).isNull())
    return item->data(0, OBJECT_START_ROLE).toInt();
  // if it has children, try the first child
  int result = INT_MAX;
  if (item->childCount() > 0)
    result = ItemsFirstObject(item->child(0), true);
  if (result < INT_MAX)
    return result;
  // if it's a layer, and there are objects after it before the next layer
  // try the first object after it
  if (item->parent() && !item->data(0, LAYER_ROLE).isNull())
  {
    int index = item->parent()->indexOfChild(item);
    if (index + 1 < item->parent()->childCount())
    {
      QTreeWidgetItem* next_item = item->parent()->child(index + 1);
      if ((next_item->data(0, LAYER_ROLE).isNull() && next_item->data(0, EFBCOPY_ROLE).isNull()) ||
          allow_siblings)
        result = ItemsFirstObject(next_item, allow_siblings);
    }
  }
  // if it's an EFB copy, and there are objects before it that aren't an EFB copy
  // keep going back to the first object before it that isn't an EFB copy
  else if (item->parent() && !item->data(0, EFBCOPY_ROLE).isNull())
  {
    int index = item->parent()->indexOfChild(item);
    while (index - 1 >= 0)
    {
      QTreeWidgetItem* prev_item = item->parent()->child(index - 1);
      if (!prev_item->data(0, EFBCOPY_ROLE).isNull())
        break;
      index--;
    }
    QTreeWidgetItem* prev_item = item->parent()->child(index);
    if (prev_item != item)
      result = ItemsFirstObject(prev_item);
  }
  // either we found our first object, or we're returning INT_MAX
  return result;
}

int ItemsLastObject(QTreeWidgetItem* item)
{
  // if it's the entire frame or sequence, play the whole thing
  if (!item->data(0, TYPE_ROLE).isNull())
  {
    int type = item->data(0, TYPE_ROLE).toInt();
    if (type == TYPE_FRAME || type == TYPE_XFBCOPY || type == TYPE_WHOLE)
      return INT_MAX - 1;
  }
  // if it's an object, problem solved
  if (!item->data(0, OBJECT_END_ROLE).isNull())
    return item->data(0, OBJECT_END_ROLE).toInt();
  // if it has children, try the last child
  int result = -1;
  if (item->childCount() > 0)
    result = ItemsFirstObject(item->child(item->childCount() - 1));
  if (result >= 0)
    return result;
  // if it's a layer, and there are objects after it before the next layer
  // try the last object after it
  if (item->parent() && !item->data(0, LAYER_ROLE).isNull())
  {
    int index = item->parent()->indexOfChild(item);
    while (index + 1 < item->parent()->childCount())
    {
      QTreeWidgetItem* next_item = item->parent()->child(index + 1);
      if ((!next_item->data(0, LAYER_ROLE).isNull()) ||
          (!next_item->data(0, EFBCOPY_ROLE).isNull()))
        break;
      index = index + 1;
    }
    QTreeWidgetItem* final_good_item = item->parent()->child(index);
    if (final_good_item != item)
      result = ItemsFirstObject(final_good_item);
  }
  // if it's an EFB copy, and there are objects before it that aren't an EFB copy
  // get the previous one
  else if (item->parent() && !item->data(0, EFBCOPY_ROLE).isNull())
  {
    int index = item->parent()->indexOfChild(item);
    if (index - 1 >= 0)
    {
      QTreeWidgetItem* prev_item = item->parent()->child(index - 1);
      if (prev_item->data(0, EFBCOPY_ROLE).isNull())
        result = ItemsFirstObject(prev_item);
    }
  }
  // either we found our last object, or we're returning -1
  return result;
}

namespace
{
class DetailCallback : public OpcodeDecoder::Callback
{
public:
  explicit DetailCallback(CPState cpmem) : m_cpmem(cpmem) {}

  OPCODE_CALLBACK(void OnCP(u8 command, u32 value))
  {
    // Note: No need to update m_cpmem as it already has the final value for this object

    const auto [name, desc] = GetCPRegInfo(command, value);
    ASSERT(!name.empty());

    text = QStringLiteral("CP  %1  %2  %3")
               .arg(command, 2, 16, QLatin1Char('0'))
               .arg(value, 8, 16, QLatin1Char('0'))
               .arg(QString::fromStdString(name));
  }

  OPCODE_CALLBACK(void OnXF(u16 address, u8 count, const u8* data))
  {
    const auto [name, desc] = GetXFTransferInfo(address, count, data);
    ASSERT(!name.empty());

    const u32 command = address | ((count - 1) << 16);

    text = QStringLiteral("XF  %1  ").arg(command, 8, 16, QLatin1Char('0'));

    for (u8 i = 0; i < count; i++)
    {
      const u32 value = Common::swap32(&data[i * 4]);

      text += QStringLiteral("%1 ").arg(value, 8, 16, QLatin1Char('0'));
    }

    text += QStringLiteral("  ") + QString::fromStdString(name);
  }

  OPCODE_CALLBACK(void OnBP(u8 command, u32 value))
  {
    const auto [name, desc] = GetBPRegInfo(command, value);
    ASSERT(!name.empty());

    text = QStringLiteral("BP  %1  %2  %3")
               .arg(command, 2, 16, QLatin1Char('0'))
               .arg(value, 6, 16, QLatin1Char('0'))
               .arg(QString::fromStdString(name));
  }
  OPCODE_CALLBACK(void OnIndexedLoad(CPArray array, u32 index, u16 address, u8 size))
  {
    const auto [desc, written] = GetXFIndexedLoadInfo(array, index, address, size);
    text = QStringLiteral("LOAD INDX %1   %2")
               .arg(QString::fromStdString(fmt::to_string(array)))
               .arg(QString::fromStdString(desc));
  }
  OPCODE_CALLBACK(void OnPrimitiveCommand(OpcodeDecoder::Primitive primitive, u8 vat,
                                          u32 vertex_size, u16 num_vertices, const u8* vertex_data))
  {
    const auto name = fmt::to_string(primitive);

    // Note that vertex_count is allowed to be 0, with no special treatment
    // (another command just comes right after the current command, with no vertices in between)
    const u32 object_prim_size = num_vertices * vertex_size;

    const u8 opcode =
        0x80 | (static_cast<u8>(primitive) << OpcodeDecoder::GX_PRIMITIVE_SHIFT) | vat;
    text = QStringLiteral("PRIMITIVE %1 (%2)  %3 vertices %4 bytes/vertex %5 total bytes")
               .arg(QString::fromStdString(name))
               .arg(opcode, 2, 16, QLatin1Char('0'))
               .arg(num_vertices)
               .arg(vertex_size)
               .arg(object_prim_size);

    // It's not really useful to have a massive unreadable hex string for the object primitives.
    // Put it in the description instead.

// #define INCLUDE_HEX_IN_PRIMITIVES
#ifdef INCLUDE_HEX_IN_PRIMITIVES
    text += QStringLiteral("   ");
    for (u32 i = 0; i < object_prim_size; i++)
    {
      text += QStringLiteral("%1").arg(vertex_data[i], 2, 16, QLatin1Char('0'));
    }
#endif
  }

  OPCODE_CALLBACK(void OnDisplayList(u32 address, u32 size))
  {
    text = QObject::tr("Call display list at %1 with size %2")
               .arg(address, 8, 16, QLatin1Char('0'))
               .arg(size, 8, 16, QLatin1Char('0'));
  }

  OPCODE_CALLBACK(void OnNop(u32 count))
  {
    if (count > 1)
      text = QStringLiteral("NOP (%1x)").arg(count);
    else
      text = QStringLiteral("NOP");
  }

  OPCODE_CALLBACK(void OnUnknown(u8 opcode, const u8* data))
  {
    using OpcodeDecoder::Opcode;
    if (static_cast<Opcode>(opcode) == Opcode::GX_CMD_UNKNOWN_METRICS)
      text = QStringLiteral("GX_CMD_UNKNOWN_METRICS");
    else if (static_cast<Opcode>(opcode) == Opcode::GX_CMD_INVL_VC)
      text = QStringLiteral("GX_CMD_INVL_VC");
    else
      text = QStringLiteral("Unknown opcode %1").arg(opcode, 2, 16);
  }

  OPCODE_CALLBACK(void OnCommand(const u8* data, u32 size)) {}

  OPCODE_CALLBACK(CPState& GetCPState()) { return m_cpmem; }

  OPCODE_CALLBACK(u32 GetVertexSize(u8 vat))
  {
    return VertexLoaderBase::GetVertexSize(GetCPState().vtx_desc, GetCPState().vtx_attr[vat]);
  }

  QString text;
  CPState m_cpmem;
};
}  // namespace

void FIFOAnalyzer::UpdateDetails()
{
  // Clearing the detail list can update the selection, which causes UpdateDescription to be called
  // immediately.  However, the object data offsets have not been recalculated yet, which can cause
  // the wrong data to be used, potentially leading to out of bounds data or other bad things.
  // Clear m_object_data_offsets first, so that UpdateDescription exits immediately.
  m_object_data_offsets.clear();
  m_detail_list->clear();
  m_search_results.clear();
  m_search_next->setEnabled(false);
  m_search_previous->setEnabled(false);
  m_search_label->clear();

  if (!FifoPlayer::GetInstance().IsPlaying())
    return;

  const auto items = m_tree_widget->selectedItems();

  if (items.isEmpty())
    return;

  // Only play the selected frame and selected objects in the game window
  int first_object = INT_MAX;
  int last_object = -1;
  int first_frame = INT_MAX;
  int last_frame = -1;
  for (int sel = 0; sel < items.count(); sel++)
  {
    if (!items[sel]->data(0, FRAME_ROLE).isNull())
    {
      int frame = items[sel]->data(0, FRAME_ROLE).toInt();
      if (frame < first_frame && frame >= 0)
        first_frame = frame;
      if (frame > last_frame && frame < INT_MAX)
        last_frame = frame;
    }
    else
    {
      first_frame = 0;
      last_frame = INT_MAX - 1;
    }
    int test = ItemsFirstObject(items[sel]);
    if (test < first_object && test >= 0)
      first_object = test;
    if (test > last_object && test < INT_MAX)
      last_object = test;
    test = ItemsLastObject(items[sel]);
    if (test < first_object && test >= 0)
      first_object = test;
    if (test > last_object && test < INT_MAX)
      last_object = test;
  }
  if (first_frame == INT_MAX)
    first_frame = 0;
  if (last_frame < 0)
    last_frame = INT_MAX - 1;
  FifoPlayer& player = FifoPlayer::GetInstance();
  player.SetObjectRangeStart(first_object);
  player.SetObjectRangeEnd(last_object);
  player.SetFrameRangeStart(first_frame);
  player.SetFrameRangeEnd(last_frame);

  if (!items[0]->data(0, LAYER_ROLE).isNull())
  {
    UpdateLayerDetails(items[0]);
    return;
  }
  if (items[0]->data(0, PART_START_ROLE).isNull())
    return;
  if (items[0]->data(0, FRAME_ROLE).isNull())
    return;

  // Actual updating of details starts here

  const u32 frame_nr = items[0]->data(0, FRAME_ROLE).toUInt();
  const u32 start_part_nr = items[0]->data(0, PART_START_ROLE).toUInt();
  const u32 end_part_nr = items[0]->data(0, PART_END_ROLE).toUInt();

  const AnalyzedFrameInfo& frame_info = player.GetAnalyzedFrameInfo(frame_nr);
  const auto& fifo_frame = player.GetFile()->GetFrame(frame_nr);

  const u32 object_start = frame_info.parts[start_part_nr].m_start;
  const u32 object_end = frame_info.parts[end_part_nr].m_end;
  const u32 object_size = object_end - object_start;

  u32 object_offset = 0;
  // NOTE: object_info.m_cpmem is the state of cpmem _after_ all of the commands in this object.
  // However, it doesn't matter that it doesn't match the start, since it will match by the time
  // primitives are reached.
  auto callback = DetailCallback(frame_info.parts[end_part_nr].m_cpmem);

  while (object_offset < object_size)
  {
    const u32 start_offset = object_offset;
    m_object_data_offsets.push_back(start_offset);

    object_offset += OpcodeDecoder::RunCommand(&fifo_frame.fifoData[object_start + start_offset],
                                               object_size - start_offset, callback);

    QString new_label =
        QStringLiteral("%1:  ").arg(object_start + start_offset, 8, 16, QLatin1Char('0')) +
        callback.text;
    m_detail_list->addItem(new_label);
  }

  // Needed to ensure the description updates when changing objects
  m_detail_list->setCurrentRow(0);
}

void FIFOAnalyzer::UpdateLayerDetails(QTreeWidgetItem* item)
{
  int frame_nr = item->data(0, FRAME_ROLE).toInt();
  int layer_nr = item->data(0, LAYER_ROLE).toInt();

  if (layer_nr != 0 || frame_nr != 0)
    return;

  // const auto& frame_info = FifoPlayer::GetInstance().GetAnalyzedFrameInfo(frame_nr);
  // const auto& fifo_frame = FifoPlayer::GetInstance().GetFile()->GetFrame(frame_nr);
  u32* p = FifoPlayer::GetInstance().GetFile()->GetXFRegs();
  p -= 0x1000;
  XFMemory* xfregs = (XFMemory*)p;
  QString new_label = DescribeViewport(&xfregs->viewport);
  m_detail_list->addItem(new_label);
  new_label = DescribeProjection(&xfregs->projection);
  m_detail_list->addItem(new_label);
}

void FIFOAnalyzer::BeginSearch()
{
  const QString search_str = m_search_edit->text();

  if (!FifoPlayer::GetInstance().IsPlaying())
    return;

  const auto items = m_tree_widget->selectedItems();

  if (items.isEmpty() || items[0]->data(0, FRAME_ROLE).isNull() ||
      items[0]->data(0, PART_START_ROLE).isNull())
  {
    m_search_label->setText(tr("Invalid search parameters (no object selected)"));
    return;
  }

  // Having PART_START_ROLE indicates that this is valid
  const int object_idx = items[0]->parent()->indexOfChild(items[0]);

  // TODO: Remove even string length limit
  if (search_str.length() % 2)
  {
    m_search_label->setText(tr("Invalid search string (only even string lengths supported)"));
    return;
  }

  const size_t length = search_str.length() / 2;

  std::vector<u8> search_val;

  for (size_t i = 0; i < length; i++)
  {
    const QString byte_str = search_str.mid(static_cast<int>(i * 2), 2);

    bool good;
    u8 value = byte_str.toUInt(&good, 16);

    if (!good)
    {
      m_search_label->setText(tr("Invalid search string (couldn't convert to number)"));
      return;
    }

    search_val.push_back(value);
  }

  m_search_results.clear();

  const u32 frame_nr = items[0]->data(0, FRAME_ROLE).toUInt();
  const u32 start_part_nr = items[0]->data(0, PART_START_ROLE).toUInt();
  const u32 end_part_nr = items[0]->data(0, PART_END_ROLE).toUInt();

  const AnalyzedFrameInfo& frame_info = FifoPlayer::GetInstance().GetAnalyzedFrameInfo(frame_nr);
  const FifoFrameInfo& fifo_frame = FifoPlayer::GetInstance().GetFile()->GetFrame(frame_nr);

  const u32 object_start = frame_info.parts[start_part_nr].m_start;
  const u32 object_end = frame_info.parts[end_part_nr].m_end;
  const u32 object_size = object_end - object_start;

  const u8* const object = &fifo_frame.fifoData[object_start];

  // TODO: Support searching for bit patterns
  for (u32 cmd_nr = 0; cmd_nr < m_object_data_offsets.size(); cmd_nr++)
  {
    const u32 cmd_start = m_object_data_offsets[cmd_nr];
    const u32 cmd_end = (cmd_nr + 1 == m_object_data_offsets.size()) ?
                            object_size :
                            m_object_data_offsets[cmd_nr + 1];

    const u8* const cmd_start_ptr = &object[cmd_start];
    const u8* const cmd_end_ptr = &object[cmd_end];

    for (const u8* ptr = cmd_start_ptr; ptr < cmd_end_ptr - length + 1; ptr++)
    {
      if (std::equal(search_val.begin(), search_val.end(), ptr))
      {
        m_search_results.emplace_back(frame_nr, object_idx, cmd_nr);
        break;
      }
    }
  }

  ShowSearchResult(0);

  m_search_label->setText(
      tr("Found %1 results for \"%2\"").arg(m_search_results.size()).arg(search_str));
}

void FIFOAnalyzer::FindNext()
{
  const int index = m_detail_list->currentRow();
  ASSERT(index >= 0);

  auto next_result =
      std::find_if(m_search_results.begin(), m_search_results.end(),
                   [index](auto& result) { return result.m_cmd > static_cast<u32>(index); });
  if (next_result != m_search_results.end())
  {
    ShowSearchResult(next_result - m_search_results.begin());
  }
}

void FIFOAnalyzer::FindPrevious()
{
  const int index = m_detail_list->currentRow();
  ASSERT(index >= 0);

  auto prev_result =
      std::find_if(m_search_results.rbegin(), m_search_results.rend(),
                   [index](auto& result) { return result.m_cmd < static_cast<u32>(index); });
  if (prev_result != m_search_results.rend())
  {
    ShowSearchResult((m_search_results.rend() - prev_result) - 1);
  }
}

void FIFOAnalyzer::ShowSearchResult(size_t index)
{
  if (m_search_results.empty())
    return;

  if (index >= m_search_results.size())
  {
    ShowSearchResult(m_search_results.size() - 1);
    return;
  }

  const auto& result = m_search_results[index];

  QTreeWidgetItem* object_item =
      m_tree_widget->topLevelItem(0)->child(result.m_frame)->child(result.m_object_idx);

  m_tree_widget->setCurrentItem(object_item);
  m_detail_list->setCurrentRow(result.m_cmd);

  m_search_next->setEnabled(index + 1 < m_search_results.size());
  m_search_previous->setEnabled(index > 0);
}

namespace
{
// TODO: Not sure whether we should bother translating the descriptions
class DescriptionCallback : public OpcodeDecoder::Callback
{
public:
  explicit DescriptionCallback(const CPState& cpmem) : m_cpmem(cpmem) {}

  OPCODE_CALLBACK(void OnBP(u8 command, u32 value))
  {
    const auto [name, desc] = GetBPRegInfo(command, value);
    ASSERT(!name.empty());

    text = QObject::tr("BP register ");
    text += QString::fromStdString(name);
    text += QLatin1Char{'\n'};

    if (desc.empty())
      text += QObject::tr("No description available");
    else
      text += QString::fromStdString(desc);
  }

  OPCODE_CALLBACK(void OnCP(u8 command, u32 value))
  {
    // Note: No need to update m_cpmem as it already has the final value for this object

    const auto [name, desc] = GetCPRegInfo(command, value);
    ASSERT(!name.empty());

    text = QObject::tr("CP register ");
    text += QString::fromStdString(name);
    text += QLatin1Char{'\n'};

    if (desc.empty())
      text += QObject::tr("No description available");
    else
      text += QString::fromStdString(desc);
  }

  OPCODE_CALLBACK(void OnXF(u16 address, u8 count, const u8* data))
  {
    const auto [name, desc] = GetXFTransferInfo(address, count, data);
    ASSERT(!name.empty());

    text = QObject::tr("XF register ");
    text += QString::fromStdString(name);
    text += QLatin1Char{'\n'};

    if (desc.empty())
      text += QObject::tr("No description available");
    else
      text += QString::fromStdString(desc);
  }

  OPCODE_CALLBACK(void OnIndexedLoad(CPArray array, u32 index, u16 address, u8 size))
  {
    const auto [desc, written] = GetXFIndexedLoadInfo(array, index, address, size);

    text = QString::fromStdString(desc);
    text += QLatin1Char{'\n'};
    switch (array)
    {
    case CPArray::XF_A:
      text += QObject::tr("Usually used for position matrices");
      break;
    case CPArray::XF_B:
      // i18n: A normal matrix is a matrix used for transforming normal vectors. The word "normal"
      // does not have its usual meaning here, but rather the meaning of "perpendicular to a
      // surface".
      text += QObject::tr("Usually used for normal matrices");
      break;
    case CPArray::XF_C:
      // i18n: Tex coord is short for texture coordinate
      text += QObject::tr("Usually used for tex coord matrices");
      break;
    case CPArray::XF_D:
      text += QObject::tr("Usually used for light objects");
      break;
    default:
      break;
    }
    text += QLatin1Char{'\n'};
    text += QString::fromStdString(written);
  }

  OPCODE_CALLBACK(void OnPrimitiveCommand(OpcodeDecoder::Primitive primitive, u8 vat,
                                          u32 vertex_size, u16 num_vertices, const u8* vertex_data))
  {
    const auto name = fmt::format("{} VAT {}", primitive, vat);

    // i18n: In this context, a primitive means a point, line, triangle or rectangle.
    // Do not translate the word primitive as if it was an adjective.
    text = QObject::tr("Primitive %1").arg(QString::fromStdString(name));
    text += QLatin1Char{'\n'};

    const auto& vtx_desc = m_cpmem.vtx_desc;
    const auto& vtx_attr = m_cpmem.vtx_attr[vat];

    u32 i = 0;
    const auto process_component = [&](VertexComponentFormat cformat, ComponentFormat format,
                                       u32 non_indexed_count, u32 indexed_count = 1) {
      u32 count;
      if (cformat == VertexComponentFormat::NotPresent)
        return;
      else if (cformat == VertexComponentFormat::Index8)
      {
        format = ComponentFormat::UByte;
        count = indexed_count;
      }
      else if (cformat == VertexComponentFormat::Index16)
      {
        format = ComponentFormat::UShort;
        count = indexed_count;
      }
      else
      {
        count = non_indexed_count;
      }

      const u32 component_size = GetElementSize(format);
      for (u32 j = 0; j < count; j++)
      {
        for (u32 component_off = 0; component_off < component_size; component_off++)
        {
          text += QStringLiteral("%1").arg(vertex_data[i + component_off], 2, 16, QLatin1Char('0'));
        }
        if (format == ComponentFormat::Float)
        {
          const float value = Common::BitCast<float>(Common::swap32(&vertex_data[i]));
          text += QStringLiteral(" (%1)").arg(value);
        }
        i += component_size;
        text += QLatin1Char{' '};
      }
      text += QLatin1Char{' '};
    };
    const auto process_simple_component = [&](u32 size) {
      for (u32 component_off = 0; component_off < size; component_off++)
      {
        text += QStringLiteral("%1").arg(vertex_data[i + component_off], 2, 16, QLatin1Char('0'));
      }
      i += size;
      text += QLatin1Char{' '};
      text += QLatin1Char{' '};
    };

    for (u32 vertex_num = 0; vertex_num < num_vertices; vertex_num++)
    {
      ASSERT(i == vertex_num * vertex_size);

      text += QLatin1Char{'\n'};
      if (vtx_desc.low.PosMatIdx)
        process_simple_component(1);
      for (auto texmtxidx : vtx_desc.low.TexMatIdx)
      {
        if (texmtxidx)
          process_simple_component(1);
      }
      process_component(vtx_desc.low.Position, vtx_attr.g0.PosFormat,
                        vtx_attr.g0.PosElements == CoordComponentCount::XY ? 2 : 3);
      const u32 normal_component_count =
          vtx_desc.low.Normal == VertexComponentFormat::Direct ? 3 : 1;
      const u32 normal_elements = vtx_attr.g0.NormalElements == NormalComponentCount::NTB ? 3 : 1;
      process_component(vtx_desc.low.Normal, vtx_attr.g0.NormalFormat,
                        normal_component_count * normal_elements,
                        vtx_attr.g0.NormalIndex3 ? normal_elements : 1);
      for (u32 c = 0; c < vtx_desc.low.Color.Size(); c++)
      {
        static constexpr Common::EnumMap<u32, ColorFormat::RGBA8888> component_sizes = {
            2,  // RGB565
            3,  // RGB888
            4,  // RGB888x
            2,  // RGBA4444
            3,  // RGBA6666
            4,  // RGBA8888
        };
        switch (vtx_desc.low.Color[c])
        {
        case VertexComponentFormat::Index8:
          process_simple_component(1);
          break;
        case VertexComponentFormat::Index16:
          process_simple_component(2);
          break;
        case VertexComponentFormat::Direct:
          process_simple_component(component_sizes[vtx_attr.GetColorFormat(c)]);
          break;
        case VertexComponentFormat::NotPresent:
          break;
        }
      }
      for (u32 t = 0; t < vtx_desc.high.TexCoord.Size(); t++)
      {
        process_component(vtx_desc.high.TexCoord[t], vtx_attr.GetTexFormat(t),
                          vtx_attr.GetTexElements(t) == TexComponentCount::ST ? 2 : 1);
      }
    }
  }

  OPCODE_CALLBACK(void OnDisplayList(u32 address, u32 size))
  {
    text = QObject::tr("No description available");
  }

  OPCODE_CALLBACK(void OnNop(u32 count)) { text = QObject::tr("does nothing (No Operation)"); }
  OPCODE_CALLBACK(void OnUnknown(u8 opcode, const u8* data))
  {
    using OpcodeDecoder::Opcode;
    if (static_cast<Opcode>(opcode) == Opcode::GX_CMD_UNKNOWN_METRICS)
      text = QStringLiteral("0x44 GX_CMD_UNKNOWN_METRICS\nzelda 4 swords calls it and checks the "
                            "metrics registers after that");
    else if (static_cast<Opcode>(opcode) == Opcode::GX_CMD_INVL_VC)
      text = QStringLiteral("0x48 GX_CMD_INVL_VC\nInvalidate (vertex cache?)");
    else
      text = QStringLiteral("Unknown opcode %1").arg(opcode, 2, 16);
  }

  OPCODE_CALLBACK(void OnCommand(const u8* data, u32 size)) {}

  OPCODE_CALLBACK(CPState& GetCPState()) { return m_cpmem; }

  OPCODE_CALLBACK(u32 GetVertexSize(u8 vat))
  {
    return VertexLoaderBase::GetVertexSize(GetCPState().vtx_desc, GetCPState().vtx_attr[vat]);
  }

  QString text;
  CPState m_cpmem;
};
}  // namespace

void FIFOAnalyzer::UpdateDescription()
{
  m_entry_detail_browser->clear();

  if (!FifoPlayer::GetInstance().IsPlaying())
    return;

  const auto items = m_tree_widget->selectedItems();

  if (items.isEmpty() || m_object_data_offsets.empty())
    return;

  if (items[0]->data(0, FRAME_ROLE).isNull() || items[0]->data(0, PART_START_ROLE).isNull())
    return;

  const u32 frame_nr = items[0]->data(0, FRAME_ROLE).toUInt();
  const u32 start_part_nr = items[0]->data(0, PART_START_ROLE).toUInt();
  const u32 end_part_nr = items[0]->data(0, PART_END_ROLE).toUInt();
  const u32 entry_nr = m_detail_list->currentRow();

  const AnalyzedFrameInfo& frame_info = FifoPlayer::GetInstance().GetAnalyzedFrameInfo(frame_nr);
  const FifoFrameInfo& fifo_frame = FifoPlayer::GetInstance().GetFile()->GetFrame(frame_nr);

  const u32 object_start = frame_info.parts[start_part_nr].m_start;
  const u32 object_end = frame_info.parts[end_part_nr].m_end;
  const u32 object_size = object_end - object_start;
  const u32 entry_start = m_object_data_offsets[entry_nr];

  auto callback = DescriptionCallback(frame_info.parts[end_part_nr].m_cpmem);
  OpcodeDecoder::RunCommand(&fifo_frame.fifoData[object_start + entry_start],
                            object_size - entry_start, callback);
  m_entry_detail_browser->setText(callback.text);
}

void FIFOAnalyzer::CheckObject(u32 frame_nr, u32 start_part_nr, u32 end_part_nr, XFMemory* xf,
                               BPMemory* bp, bool* projection_set, bool* viewport_set,
                               bool* scissor_set, bool* scissor_offset_set, bool* efb_copied,
                               QString* desc)
{
  *projection_set = false;
  *viewport_set = false;
  *scissor_set = false;
  *scissor_offset_set = false;
  *efb_copied = false;

  if (!FifoPlayer::GetInstance().IsPlaying())
    return;

  const AnalyzedFrameInfo& frame_info = FifoPlayer::GetInstance().GetAnalyzedFrameInfo(frame_nr);
  const auto& fifo_frame = FifoPlayer::GetInstance().GetFile()->GetFrame(frame_nr);

  const u32 object_start = frame_info.parts[start_part_nr].m_start;
  const u32 object_end = frame_info.parts[end_part_nr].m_end;
  const u32 object_size = object_end - object_start;

  u32 object_offset = 0;
  // NOTE: object_info.m_cpmem is the state of cpmem _after_ all of the commands in this object.
  // However, it doesn't matter that it doesn't match the start, since it will match by the time
  // primitives are reached.
  auto callback = SimulateCallback(frame_info.parts[end_part_nr].m_cpmem, xf, bp);

  while (object_offset < object_size)
  {
    const u32 start_offset = object_offset;
    m_object_data_offsets.push_back(start_offset);

    object_offset += OpcodeDecoder::RunCommand(&fifo_frame.fifoData[object_start + start_offset],
                                               object_size - start_offset, callback);
  }
  callback.finish_desc();

  *projection_set = callback.projection_set;
  *viewport_set = callback.viewport_set;
  *scissor_set = callback.scissor_set;
  *scissor_offset_set = callback.scissor_offset_set;
  *efb_copied = callback.efb_copied;
  *desc = callback.text;
}
