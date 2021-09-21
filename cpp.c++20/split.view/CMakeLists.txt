cmake_minimum_required (VERSION 3.21)

project (split)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-11)
set (CMAKE_BUILD_TYPE Release)

set (SOURCES
  main.cpp
)

#include_directories (
#  /usr/local/include 
#  ${CMAKE_CURRENT_SOURCE_DIR}
#)

link_directories (/usr/local/lib)

add_executable (split ${SOURCES})
