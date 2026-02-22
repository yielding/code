cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
)

# Platform detection
if (APPLE)
  set (HOMEBREW_PREFIX "/opt/homebrew")

  if (EXISTS ${HOMEBREW_PREFIX})
    include_directories (
      ${HOMEBREW_PREFIX}/include
    )
    link_directories (
      ${HOMEBREW_PREFIX}/lib
    )
  endif()

elseif (UNIX AND NOT APPLE)
  include_directories (
    /usr/local/include
    /usr/include
  )
  link_directories (
    /usr/local/lib
    /usr/lib
    /usr/lib/x86_64-linux-gnu
  )
endif()

include_directories (
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable (main
  ${SOURCES}
)
