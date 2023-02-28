cmake_minimum_required(VERSION 3.22.2)

project(image.overlay)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable(image.overlay main.cpp)

target_link_libraries(image.overlay 
  ${OpenCV_LIBS}
)
