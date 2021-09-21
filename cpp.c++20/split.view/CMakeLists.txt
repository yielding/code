cmake_minimum_required (VERSION 3.21)

project (split)

set (CMAKE_CXX_STANDARD 23)

set (SOURCES
  main.cpp
)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

target_link_libraries ()

add_executable (split ${SOURCES})
