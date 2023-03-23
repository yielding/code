cmake_minimum_required (VERSION 3.26)

project (active.object)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  active.object.cpp
)

find_package(Boost 1.81 REQUIRED thread)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

target_link_libraries (
  Boost::thread
  )

add_executable (active.object ${SOURCES})
