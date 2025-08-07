cmake_minimum_required (VERSION 3.30.0)

project(future.promise.exception)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  future.promise.exception.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib 
  /opt/homebrew/lib
)

add_executable(future.promise.exception 
  ${SOURCES}
)

target_link_libraries (future.promise.exception 
  d_classic
)

