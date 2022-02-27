cmake_minimum_required (VERSION 3.15)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (avio.read)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  avio.reading.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

add_executable (avio.read ${SOURCES})

target_link_libraries (avio.read
  avformat avcodec avutil avdevice swscale swresample
)

