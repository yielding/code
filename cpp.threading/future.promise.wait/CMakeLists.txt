cmake_minimum_required (VERSION 3.30.0)

project (future.promise.wait)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  future.promise.wait.cpp
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

add_executable(future.promise.wait 
  ${SOURCES}
)

target_link_libraries (future.promise.wait
  d_classic
)

