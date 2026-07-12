cmake_minimum_required (VERSION 3.30.0)

# Default compiler: a recent g++ on Linux; on macOS use the toolchain default
# (clang++/xcrun) so a missing g++-16 does not break the configure step.
if (NOT DEFINED CMAKE_CXX_COMPILER AND NOT DEFINED ENV{CXX})
  if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    set (CMAKE_CXX_COMPILER g++-16)
  endif()
endif()

project (md.analyzer CXX)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

# libmruby.a is built without -fPIC; PIE link fails on Linux
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_link_options (-no-pie)
endif()

################################################################################
# Locate mruby
#
# Default: mruby 4.0.0 under ~/.rubies (macOS and Linux). Override with
#   cmake -B build -DMRUBY_ROOT=/path/to/mruby      (custom install prefix), or
#   cmake -B build -DMRUBY_VERSION=x.y.z            (different ~/.rubies version)
################################################################################
if (NOT DEFINED MRUBY_VERSION)
  set (MRUBY_VERSION 4.0.0)
endif()

if (NOT DEFINED MRUBY_ROOT)
  set (MRUBY_ROOT $ENV{HOME}/.rubies/mruby-${MRUBY_VERSION})
endif()

set (MRUBY_INCLUDE_DIRS
  ${MRUBY_ROOT}/include
  ${MRUBY_ROOT}/build/host/include
)
set (MRUBY_LIB_DIR ${MRUBY_ROOT}/build/host/lib)

if (NOT EXISTS ${MRUBY_LIB_DIR}/libmruby.a)
  message (FATAL_ERROR
    "libmruby.a not found under ${MRUBY_LIB_DIR}. "
    "Set -DMRUBY_ROOT=<mruby install> or -DMRUBY_VERSION=<x.y.z>.")
endif()

set (SOURCES
  main.cpp
  data_store_ext.cpp
  data_store.cpp
  file_system_ext.cpp
  file_system.cpp
  node_ext.cpp
  node.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ${MRUBY_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../mrubybind
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
  ${MRUBY_LIB_DIR}
)

add_executable(md.analyzer
  ${SOURCES}
)

target_link_libraries(md.analyzer
  mruby
)
