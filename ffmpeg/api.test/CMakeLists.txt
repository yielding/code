cmake_minimum_required (VERSION 3.30.0)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (api.test)

set (SOURCES
  main.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  /Users/yielding/develop/include
  /usr/local/include 
  /opt/homebrew/include
  ~/develop/include
  ~/develop/vendor/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (api.test ${SOURCES})

target_link_libraries (api.test
  avformat avcodec avutil avdevice swscale swresample avfilter
)