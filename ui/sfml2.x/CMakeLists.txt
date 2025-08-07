cmake_minimum_required (VERSION 3.28)

project (sfml-app)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER g++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
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

add_executable (sfml-app ${SOURCES})

target_link_libraries (sfml-app
  sfml-graphics
  sfml-window
  sfml-system
)
# or library
# add_library (__PROJECT__ STATIC ${SOURCES})
# add_library (__PROJECT__ SHARED ${SOURCES})

# https://gist.github.com/luncliff/6e2d4eb7ca29a0afd5b592f72b80cb5c
