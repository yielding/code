cmake_minimum_required (VERSION 3.30.0)

project (daemon)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  daemon.cpp
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

add_executable (daemon ${SOURCES})

target_link_libraries (daemon
  # lib name 1
  # lib name 2
)

# or library
# add_library (daemon STATIC ${SOURCES})
# add_library (daemon SHARED ${SOURCES})

# https://gist.github.com/luncliff/6e2d4eb7ca29a0afd5b592f72b80cb5c
