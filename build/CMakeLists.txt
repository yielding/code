cmake_minimum_required (VERSION 3.21)

project (__PROJECT__)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  __SOURCE__.cpp
)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

target_link_libraries ()

add_executable (__PROJECT__ ${SOURCES})
