cmake_minimum_required (VERSION 3.15)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Qt5Core REQUIRED)
find_package(Qt5Widgets REQUIRED)

set (SOURCES
  main.cpp
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

add_executable(main ${SOURCES})
qt5_use_modules(main Core Widgets)
