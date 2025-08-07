cmake_minimum_required(VERSION 3.30.0)

project(image.overlay)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(OpenCV REQUIRED)

include_directories (
  /usr/include 
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable(image.overlay main.cpp)

target_link_libraries(image.overlay 
  ${OpenCV_LIBS}
)
