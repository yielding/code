cmake_minimum_required (VERSION 3.30.0)

project (cpp.api3)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  cpp.api3.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  /Users/yielding/develop/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib 
  /opt/homebrew/lib
)

add_executable (cpp.api3 ${SOURCES})

target_link_libraries(cpp.api3
  sqlite3)
