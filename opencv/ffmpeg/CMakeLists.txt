CMAKE_MINIMUM_REQUIRED (VERSION 3.15)

SET (CMAKE_CXX_STANDARD 20)
SET (CMAKE_EXPORT_COMPILE_COMMANDS ON)

PROJECT (ffmpeg.opencv)

SET (SOURCES
  ffmpeg.opencv.cpp
)

FIND_PACKAGE(OpenCV REQUIRED)

INCLUDE_DIRECTORIES (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

LINK_DIRECTORIES (/usr/local/lib)

LINK_LIBRARIES (
  ${OpenCV_LIBS}
  avutil
  avformat
  avcodec
  avutil
  swscale
)

ADD_EXECUTABLE (ffmpeg.opencv ${SOURCES})
