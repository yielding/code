cmake_minimum_required (VERSION 3.12)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Qt5Core REQUIRED)
find_package(Qt5Charts REQUIRED)

set (SOURCES
  main.cpp
  callout.cpp
  view.cpp
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

add_executable(callout ${SOURCES})
qt5_use_modules(callout Core Charts)
