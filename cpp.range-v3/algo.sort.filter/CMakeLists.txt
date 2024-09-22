cmake_minimum_required (VERSION 3.30)

project (algo.sort.filter)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  algo.sort.filter.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
)

link_directories (/usr/local/lib)

add_executable (algo.sort.filter ${SOURCES})
