cmake_minimum_required(VERSION 3.22.2)

project(deblur)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories (
  /usr/local/include 
  /usr/local/opt/opencv/include/opencv4
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/opt/opencv/lib
)

# add_subdirectory(src)

add_executable(deblur main.cpp)

target_link_libraries(deblur 
  opencv_core
  opencv_imgproc
  opencv_highgui
)
