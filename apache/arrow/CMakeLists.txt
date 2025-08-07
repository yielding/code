cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  main.cpp
)

find_package (Arrow REQUIRED)

include_directories (
  ${HOME}/develop/include
  /usr/local/include
  /opt/homebrew/include
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
  Arrow::arrow_shared
)
