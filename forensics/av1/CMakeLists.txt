cmake_minimum_required(VERSION 3.28)

project(extract.av1)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  extract.av1.cpp
  ~/develop/src/file_util.cpp
  ~/develop/src/byte_buffer.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ~/develop/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (extract.av1
  ${SOURCES}
)
