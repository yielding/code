cmake_minimum_required (VERSION 3.27)

project (sync2.client)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_CXX_COMPILER g++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  sync2.client.cpp
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

add_executable ( sync2.client
  ${SOURCES}
)
