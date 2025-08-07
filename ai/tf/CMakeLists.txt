cmake_minimum_required (VERSION 3.15)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (SOURCES
  tf.cpp
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR} 
  /usr/local/include
  /usr/local/include/google/tensorflow
  /usr/local/include/eigen3
)

link_directories (/usr/local/lib)
link_libraries (tensorflow_cc tensorflow_framework)

add_executable(tf ${SOURCES})
