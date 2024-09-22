cmake_minimum_required (VERSION 3.30)

project (view.filter.proj)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  view.filter.proj.cpp
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

add_executable(view.filter.proj 
  ${SOURCES}
)