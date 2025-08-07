cmake_minimum_required (VERSION 3.30.0)

project (thread.pool)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  thread.pool.cpp
  /Users/yielding/develop/src/file_util.cpp
)

include_directories (
  ${CMAKE_CURRENT_SOURCE_DIR}
  /usr/local/include 
  /opt/homebrew/include
  /Users/yielding/develop/include
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (thread.pool ${SOURCES})

target_link_libraries (thread.pool
  zmq
)
