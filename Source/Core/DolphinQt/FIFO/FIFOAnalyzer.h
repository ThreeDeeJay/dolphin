// Copyright 2018 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <vector>

#include <QWidget>

#include "Common/CommonTypes.h"

class QGroupBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;
class QSplitter;
class QTextBrowser;
class QTreeWidget;
class QTreeWidgetItem;
struct Viewport;
struct Projection;
struct XFMemory;
struct BPMemory;

class FIFOAnalyzer final : public QWidget
{
  Q_OBJECT

public:
  explicit FIFOAnalyzer();
  ~FIFOAnalyzer();

  void Update();
  QString DescribeViewport(Viewport* viewport);
  QString DescribeScissor();
  QString DescribeProjection(Projection* proj);
  QString DescribeLayer(bool set_viewport, bool set_scissor, bool set_projection);
  QString DescribeEFBCopy();

private:
  void CreateWidgets();
  void ConnectWidgets();

  void BeginSearch();
  void FindNext();
  void FindPrevious();

  void ShowSearchResult(size_t index);

  void UpdateTree();
  void UpdateDetails();
  void UpdateLayerDetails(QTreeWidgetItem* item);
  void UpdateDescription();
  void CheckObject(u32 frame_nr, u32 start_part_nr, u32 end_part_nr, XFMemory* xf, BPMemory* bp,
                   bool* projection_set, bool* viewport_set, bool* scissor_set,
                   bool* scissor_offset_set, bool* efb_copied);

  QTreeWidget* m_tree_widget;
  QListWidget* m_detail_list;
  QTextBrowser* m_entry_detail_browser;
  QSplitter* m_object_splitter;

  // Search
  QGroupBox* m_search_box;
  QLineEdit* m_search_edit;
  QPushButton* m_search_new;
  QPushButton* m_search_next;
  QPushButton* m_search_previous;
  QLabel* m_search_label;
  QSplitter* m_search_splitter;

  struct SearchResult
  {
    constexpr SearchResult(u32 frame, u32 object_idx, u32 cmd)
        : m_frame(frame), m_object_idx(object_idx), m_cmd(cmd)
    {
    }
    const u32 m_frame;
    // Index in tree view.  Does not correspond with object numbers or part numbers.
    const u32 m_object_idx;
    const u32 m_cmd;
  };

  // Offsets from the start of the first part in an object for each command within the currently
  // selected object.
  std::vector<int> m_object_data_offsets;

  std::vector<SearchResult> m_search_results;
};
