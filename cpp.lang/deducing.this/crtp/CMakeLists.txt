cmake_minimum_required (VERSION 3.28.1)

project (matrix)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "/opt/homebrew/bin/g++-14")

set (SOURCES
  matrix.cpp
)

include_directories (
  /opt/homebrew/include
  /usr/local/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /opt/homebrew/lib
  /usr/local/lib
)

add_executable (matrix
  ${SOURCES}
)