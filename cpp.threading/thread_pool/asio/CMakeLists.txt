cmake_minimum_required (VERSION 3.30.0)

project (thread.pool)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ~/develop/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (thread.pool ${SOURCES})

target_link_libraries (thread.pool
  boost_thread-mt
  boost_system-mt
)
