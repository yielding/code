cmake_minimum_required (VERSION 3.26)

project (parallel_sort)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  parallel_sort.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (parallel_sort ${SOURCES})

target_link_libraries (parallel_sort 
  tbb)
