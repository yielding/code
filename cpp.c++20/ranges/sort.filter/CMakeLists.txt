cmake_minimum_required (VERSION 3.26)

project (sort.filter)

set (CMAKE_CXX_STANDARD 23)

set (SOURCES
  sort.filter.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
)

link_directories (/usr/local/lib)

add_executable (sort.filter ${SOURCES})
