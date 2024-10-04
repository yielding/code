cmake_minimum_required(VERSION 3.30)

project(main)

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

add_executable(main main.cpp)

target_link_libraries(main
  ${OpenCV_LIBS}
)
