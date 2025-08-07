cmake_minimum_required (VERSION 3.30.0)

project (reduce)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  reduce.cpp
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

add_executable (reduce
  ${SOURCES}
)

target_link_libraries (reduce
  pthread
  tbb 
)
