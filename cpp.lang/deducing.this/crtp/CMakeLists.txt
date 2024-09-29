cmake_minimum_required (VERSION 3.30)

project (matrix)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

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