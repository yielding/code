cmake_minimum_required (VERSION 3.28)

project (jthread)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
# set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -fexperimental-library")

set (SOURCES
  jthread.cpp
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

add_executable (jthread ${SOURCES})

target_link_libraries (jthread
  # lib name 1
  # lib name 2
)

