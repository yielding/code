CMAKE_MINIMUM_REQUIRED (VERSION 3.15)

PROJECT (ffmpeg_debug)

SET (CMAKE_CXX_STANDARD 20)
SET (CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET (SOURCES
  ffmpeg.c
  ffmpeg_opt.c
  ffmpeg_hw.c
  ffmpeg_filter.c
  ffmpeg_videotoolbox.c
  cmdutils.c
)

FIND_PACKAGE(OpenCV REQUIRED)

INCLUDE_DIRECTORIES (
  /Users/yielding/opensource/ffmpeg
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
  avfilter
)

ADD_EXECUTABLE (ffmpeg_debug ${SOURCES})
