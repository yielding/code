cmake_minimum_required (VERSION 3.30.0)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

project (extract2.jpeg)

set (SOURCES
  extract.jpeg2.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  ${HOME}/develop/vendor/include
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (${CMAKE_PROJECT_NAME} ${SOURCES})

target_link_libraries (${CMAKE_PROJECT_NAME}
  avformat avcodec avutil avdevice swscale swresample avfilter
  jpeg
)