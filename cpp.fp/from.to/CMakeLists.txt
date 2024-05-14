cmake_minimum_required (VERSION 3.28)

project(from_to)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  from_to.cpp
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

add_executable (from_to 
  ${SOURCES}
)