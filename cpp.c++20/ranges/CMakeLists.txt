cmake_minimum_required (VERSION 3.21)

project (sort)

set (CMAKE_CXX_STANDARD 23)

set (SOURCES
  sort.ranges.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
)

link_directories (/usr/local/lib)

add_executable (sort ${SOURCES})
