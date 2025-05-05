cmake_minimum_required (VERSION 3.30)

project (action.unstable_remove_if)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  action.unstable_remove_if.cpp
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

add_executable(action.unstable_remove_if 
  ${SOURCES}
)
