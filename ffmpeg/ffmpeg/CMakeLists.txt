cmake_minimum_required (VERSION 3.21.0)

project (ffmpeg_debug)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  ffmpeg.c
  ffmpeg_opt.c
  ffmpeg_hw.c
  ffmpeg_filter.c
  ffmpeg_videotoolbox.c
  cmdutils.c
)

find_package (OpenCV REQUIRED)

include_directories (
  /Users/yielding/opensource/ffmpeg
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (/usr/local/lib)

add_executable (ffmpeg_debug ${SOURCES})

target_link_libraries (ffmpeg_debug
  avformat
  avcodec
  avutil
  avdevice
  swscale
  swresample
  avfilter
)
