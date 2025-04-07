cmake_minimum_required(VERSION 3.30.0)

project (sendfile)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_definitions(-DPOSIX)
add_definitions(-D__APPLE__)

include_directories (
  /Users/yielding/develop/include
  /Users/yielding/develop/vendor/include
  /opt/homebrew/include
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable(server server.cpp)
add_executable(client client.cpp)