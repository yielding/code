cmake_minimum_required (VERSION 3.22)

project (main)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

set (SOURCES main.cpp)

find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable (main ${SOURCES})

target_link_libraries (main 
  ${OpenCV_LIBS}
)
