cmake_minimum_required (VERSION 3.12)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (SOURCES
  optional.cpp
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

add_executable(optional ${SOURCES})
