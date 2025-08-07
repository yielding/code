cmake_minimum_required(VERSION 3.28)

project(fold.left.comma)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  fold.left.comma.cpp
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

add_executable (fold.left.comma
  ${SOURCES}
)

