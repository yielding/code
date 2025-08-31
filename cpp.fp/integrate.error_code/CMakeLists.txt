cmake_minimum_required(VERSION 3.20)

project(expected_util_full_demo CXX)
set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

add_executable(demo demo.cpp)
