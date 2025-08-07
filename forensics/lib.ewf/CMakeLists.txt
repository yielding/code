cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  main.cpp
)

include_directories (
  ${HOME}/develop/include
  /usr/local/include
  /usr/include/c++/v1
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (${CMAKE_PROJECT_NAME}
  ${SOURCES}
)

target_link_libraries (${CMAKE_PROJECT_NAME}
  ewf
)
