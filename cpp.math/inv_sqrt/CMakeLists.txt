cmake_minimum_required (VERSION 3.26)

project (inv_sqrt)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  inv_sqrt.cpp
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

add_executable (inv_sqrt ${SOURCES})
