cmake_minimum_required(VERSION 3.14..4.2)
project(zlib-rs C)
enable_language(C)

find_program(CARGO cargo REQUIRED)

set(CARGO_TARGET_DIR "${CMAKE_CURRENT_BINARY_DIR}")
set(RUSTFLAGS "${RUSTFLAGS} -Awarnings -Cllvm-args=-enable-dfa-jump-thread")

set(ZLIB_RS_LIB_RELEASE "${CARGO_TARGET_DIR}/release/libz_rs.${CMAKE_STATIC_LIBRARY_SUFFIX}")
set(ZLIB_RS_LIB_DEBUG "${CARGO_TARGET_DIR}/debug/libz_rs.${CMAKE_STATIC_LIBRARY_SUFFIX}")
set(ZLIB_RS_DIR "${CMAKE_SOURCE_DIR}/Externals/zlib-rs/zlib-rs/libz-rs-sys-cdylib")

# We set CACHE ZLIB variables for find_package used by other projects
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set(ZLIB_RS_LIB "${ZLIB_RS_LIB_RELEASE}")
    set(ZLIB_INCLUDE_DIR "${ZLIB_RS_LIB_RELEASE}" CACHE STRING "Path to zlib include directory")
    set(CARGO_PROFILE "release")
else()
    set(ZLIB_RS_LIB "${ZLIB_RS_LIB_DEBUG}")
    set(ZLIB_INCLUDE_DIR "${ZLIB_RS_LIB_DEBUG}" CACHE STRING "Path to zlib include directory")
    set(CARGO_PROFILE "debug")
endif()
set(ZLIB_LIBRARY ZLIB::ZLIB CACHE STRING "Path to zlib library")

add_custom_target(
    zlib-rs-compat
    COMMAND ${CMAKE_COMMAND} -E env
        "CARGO_TARGET_DIR=${CARGO_TARGET_DIR}"
        "RUSTFLAGS=${RUSTFLAGS}"
        ${CARGO} build --profile=${CARGO_PROFILE}
    WORKING_DIRECTORY ${ZLIB_RS_DIR}
    COMMENT "Building zlib-rs using Cargo"
    BYPRODUCTS "${ZLIB_RS_LIB}"
)

add_library(zlib-rs STATIC IMPORTED GLOBAL)
set_target_properties(zlib-rs PROPERTIES
    IMPORTED_LOCATION_RELEASE "${ZLIB_RS_LIB_RELEASE}"
    IMPORTED_LOCATION_DEBUG "${ZLIB_RS_LIB_DEBUG}"
		INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/zlib-rs/libz-rs-sys/include"
)

add_dependencies(zlib-rs zlib-rs-compat)

add_library(ZLIB::ZLIB ALIAS zlib-rs)
