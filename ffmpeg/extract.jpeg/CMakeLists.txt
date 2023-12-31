cmake_minimum_required (VERSION 3.27)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_CXX_COMPILER g++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (extract.jpeg)

set (SOURCES
  extract.jpeg.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  /Users/yielding/develop/vendor/include
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (extract.jpeg ${SOURCES})

target_link_libraries (extract.jpeg
  avformat avcodec avutil avdevice swscale swresample avfilter
)