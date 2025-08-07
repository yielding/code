cmake_minimum_required (VERSION 3.28)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER g++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (avio.read)

set (SOURCES
  avio.reading.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (avio.read ${SOURCES})

target_link_libraries (avio.read
  avformat avcodec avutil avdevice swscale swresample
)
