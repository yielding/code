cmake_minimum_required(VERSION 3.22.2)

project(polling)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories (
  /opt/homebrew/include
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable(pull pollin.pull.cpp)
add_executable(push pollin.push.cpp)

target_link_libraries(pull zmq)
target_link_libraries(push zmq)

