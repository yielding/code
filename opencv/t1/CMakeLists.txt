cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES main.cpp)

find_package(OpenCV REQUIRED)

include_directories (
  /usr/include 
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable (main ${SOURCES})

target_link_libraries (main 
  ${OpenCV_LIBS}
)
