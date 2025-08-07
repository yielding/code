cmake_minimum_required (VERSION 3.28)

project (custom.delete)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

add_executable (custom.delete ${SOURCES})
