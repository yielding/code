cmake_minimum_required (VERSION 3.22)

project (arith)

set (CMAKE_CXX_STANDARD 23)

set (SOURCES
  add.cpp
  sub.cpp
  main.cpp
)

include_directories (
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable (arith ${SOURCES})
