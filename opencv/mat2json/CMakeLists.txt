cmake_minimum_required(VERSION 3.22.2)

project(mat2json)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

find_package(OpenCV REQUIRED)

include_directories (
  /opt/homebrew/include
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories(
  /opt/homebrew/lib
)

add_executable(mat2json main.cpp)

target_link_libraries(mat2json
  ${OpenCV_LIBS}
)
