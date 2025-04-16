cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Boost 1.82 COMPONENTS program_options REQUIRED)

# add_subdirectory(src)

add_executable (main
  main.cpp)
