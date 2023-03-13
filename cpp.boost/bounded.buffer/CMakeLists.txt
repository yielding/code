cmake_minimum_required (VERSION 3.22.2)

project (bounded.buffer)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  bounded.buffer.cpp
)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

add_executable (bounded.buffer ${SOURCES})
