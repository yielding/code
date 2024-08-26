cmake_minimum_required (VERSION 3.30)

project (decompose)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  decompose.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  /opt/homebrew/opt/icu4c/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
  /opt/homebrew/opt/icu4c/lib
)

add_executable (decompose
  ${SOURCES}
)

target_link_libraries (decompose
  icuuc icudata
)

