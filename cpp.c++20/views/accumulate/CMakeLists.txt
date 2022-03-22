cmake_minimum_required (VERSION 3.21)

project (fibo)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  fibo.cpp
)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

add_executable (fibo ${SOURCES})
