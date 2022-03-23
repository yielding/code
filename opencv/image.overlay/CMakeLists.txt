cmake_minimum_required(VERSION 3.22.2)

project(image.overlay)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

#find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  "$ENV{OPENCV_HOME}/include/opencv4"
  ${CMAKE_CURRENT_SOURCE_DIR}
)

# add_subdirectory(src)

link_directories(
  "$ENV{OPENCV_HOME}/lib"
)

add_executable(image.overlay main.cpp)

target_link_libraries(image.overlay 
  opencv_core
  opencv_imgcodecs
  opencv_highgui
  opencv_imgproc
)
