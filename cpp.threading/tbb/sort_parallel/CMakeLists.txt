cmake_minimum_required (VERSION 3.30.0)

project (sort)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  sort.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${_INCLUDE_DIRS}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (sort
  ${SOURCES}
)

target_link_libraries (sort
  tbb
)
