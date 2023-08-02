cmake_minimum_required(VERSION 3.26)

project(cpp.stdio)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

set (SOURCES
  cpp.stdio.cpp
)

find_package (OpenCV REQUIRED)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (cpp.stdio
  ${SOURCES}
)

target_link_libraries(cpp.stdio
  ${OpenCV_LIBS}
)
