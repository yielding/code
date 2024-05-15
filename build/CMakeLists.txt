cmake_minimum_required (VERSION 3.28)

project (__PROJECT__)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  __SOURCE__.cpp
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

add_executable (__PROJECT__ ${SOURCES})

target_link_libraries (__PROJECT__
  # lib name 1
  # lib name 2
)

# or library
# add_library (__PROJECT__ STATIC ${SOURCES})
# add_library (__PROJECT__ SHARED ${SOURCES})

# https://gist.github.com/luncliff/6e2d4eb7ca29a0afd5b592f72b80cb5c
