cmake_minimum_required (VERSION 3.22.2)

project (scalar)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  scalar.cpp
)

find_package (OpenCV REQUIRED)

include_directories (
  /usr/local/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
)

add_executable (scalar ${SOURCES})

target_link_libraries (scalar
  ${OpenCV_LIBS}
  boost_serialization
  boost_iostreams
)
