cmake_minimum_required(VERSION 3.22.2)

project(main)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Boost 1.78 COMPONENTS program_options REQUIRED)

# add_subdirectory(src)

add_executable(main
  main.cpp)
