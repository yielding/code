cmake_minimum_required (VERSION 3.30.0)

project (yolo_detect)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package (OpenCV REQUIRED COMPONENTS core imgproc imgcodecs dnn highgui)
find_package (PkgConfig REQUIRED)
pkg_check_modules (FFMPEG REQUIRED
  libavformat
  libavcodec
  libavutil
  libswscale
)

add_executable (${CMAKE_PROJECT_NAME}
  main.cpp
)

target_include_directories (${CMAKE_PROJECT_NAME} PRIVATE
  ${OpenCV_INCLUDE_DIRS}
  ${FFMPEG_INCLUDE_DIRS}
)

target_link_directories (${CMAKE_PROJECT_NAME} PRIVATE
  ${FFMPEG_LIBRARY_DIRS}
)

target_link_libraries (${CMAKE_PROJECT_NAME} PRIVATE
  ${OpenCV_LIBS}
  ${FFMPEG_LIBRARIES}
)

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options (${CMAKE_PROJECT_NAME} PRIVATE
    -Wall -Wextra -Wpedantic
  )
endif ()
