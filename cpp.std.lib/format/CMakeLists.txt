cmake_minimum_required(VERSION 3.28)

project (format)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  format.cpp
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

add_executable(format 
  ${SOURCES}
)

target_link_libraries (format
  d_classic
)

cmake_minimum_required (VERSION 3.28)

project (ProjectName)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  ProjectName.cpp
  
)

find_package(library)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${library_INCLUDE_DIRS}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (ProjectName
  ${SOURCES}
)

target_link_libraries (ProjectName
# lib name 1
  ${library_LIBRARIES}
)
