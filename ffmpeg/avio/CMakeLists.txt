CMAKE_MINIMUM_REQUIRED (VERSION 3.15)

PROJECT (avio.read)

SET (CMAKE_CXX_STANDARD 20)
SET (CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET (SOURCES
  avio.reading.cpp
)

FIND_PACKAGE(OpenCV REQUIRED)

INCLUDE_DIRECTORIES (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

LINK_DIRECTORIES (/usr/local/lib)

LINK_LIBRARIES (
  avformat
  avcodec
  avutil
  avdevice
  swscale
  swresample
)

ADD_EXECUTABLE (avio.read ${SOURCES})
