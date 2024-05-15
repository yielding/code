cmake_minimum_required (VERSION 3.28)

project (active.object)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  active.object.cpp
)

find_package(Boost 1.81 REQUIRED thread)

include_directories (
  /opt/homebrew/include
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (active.object ${SOURCES})
