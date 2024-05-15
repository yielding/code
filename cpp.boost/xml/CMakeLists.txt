cmake_minimum_required (VERSION 3.28)

project (main)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Boost 1.82 COMPONENTS program_options REQUIRED)

# add_subdirectory(src)

add_executable (main
  main.cpp)
