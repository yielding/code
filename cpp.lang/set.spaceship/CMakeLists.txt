cmake_minimum_required(VERSION 3.28)

project(set.spaceship)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable(set.spaceship
  main.cpp
)
