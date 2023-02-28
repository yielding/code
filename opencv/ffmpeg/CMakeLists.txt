cmake_minimum_required (VERSION 3.22.2)

project(ffmpeg.opencv)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_COMPILER "g++")

set(SOURCES
  ffmpeg.opencv.cpp
)

find_package(OpenCV REQUIRED)

# ffmpeg
find_path(SWSCALE_INCLUDE_DIR libswscale/swscale.h)
find_library(SWSCALE_LIBRARY swscale)
find_path(AVFORMAT_INCLUDE_DIR libavformat/avformat.h)
find_library(AVFORMAT_LIBRARY avformat)
find_path(AVCODEC_INCLUDE_DIR libavcodec/avcodec.h)
find_library(AVCODEC_LIBRARY avcodec)
find_path(AVUTIL_INCLUDE_DIR libavutil/avutil.h)
find_library(AVUTIL_LIBRARY avutil)

include_directories(
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

add_executable(ffmpeg.opencv
  ${SOURCES}
)

target_link_libraries (ffmpeg.opencv
  ${OpenCV_LIBS}
  ${SWSCALE_LIBRARY} 
  ${AVFORMAT_LIBRARY}
  ${AVCODEC_LIBRARY}
  ${AVUTIL_LIBRARY}
  z
)
