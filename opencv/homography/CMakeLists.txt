cmake_minimum_required (VERSION 3.22.2)

project (homography)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (CMAKE_CXX_COMPILER "g++")

#find_package(OpenCV REQUIRED)

include_directories (
  /usr/local/include 
  "$ENV{OPENCV_HOME}/include/opencv4"
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories(
  "$ENV{OPENCV_HOME}/lib"
)

add_executable (homography main.cpp)

target_link_libraries (homography
  opencv_core
  opencv_imgcodecs
  opencv_highgui
  opencv_imgproc
  opencv_calib3d
  opencv_features2d
)
