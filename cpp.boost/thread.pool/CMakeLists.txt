cmake_minimum_required (VERSION 3.28)

project (thread.pool)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
)

find_package(Boost 1.85 REQUIRED thread)

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
  Boost::thread
)
