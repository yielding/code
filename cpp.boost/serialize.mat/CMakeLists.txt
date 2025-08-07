cmake_minimum_required (VERSION 3.30.0)

project (mat.serialize)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  driver.cpp
)

find_package(OpenCV REQUIRED)
find_package(Boost 1.88 REQUIRED)

include_directories (
  ${HOME}/develop/include
  /usr/local/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (mat.serialize
  ${SOURCES}
)

target_link_libraries (mat.serialize
  ${OpenCV_LIBS}
  boost_serialization
  boost_iostreams
)
