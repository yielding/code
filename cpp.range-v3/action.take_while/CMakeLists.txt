cmake_minimum_required (VERSION 3.30)

project(action.take_while)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  action.take_while.cpp
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

add_executable(action.take_while 
  ${SOURCES}
)
