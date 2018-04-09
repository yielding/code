cmake_minimum_required(VERSION 3.11)

set(CMAKE_CXX_STANDARD 17)

INCLUDE_DIRECTORIES(/usr/local/opt/opencv@3/include /usr/local/include)

LINK_DIRECTORIES(/usr/local/opt/opencv@3/lib )

FIND_LIBRARY(OPENCL OpenCL)

LINK_LIBRARIES(opencv_core opencv_imgproc opencv_highgui ${OPENCL})

ADD_EXECUTABLE(kmeans kmeans.cpp)
