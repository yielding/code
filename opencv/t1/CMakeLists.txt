cmake_minimum_required (VERSION 3.15.3)

project (t1)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES main.cpp)

find_package(OpenCV REQUIRED)

include_directories (
/usr/local/include
/home/yielding/anaconda3/envs/opencv/include/opencv4
${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable (t1 ${SOURCES})

target_link_libraries (t1 ${OpenCV_LIBS})
