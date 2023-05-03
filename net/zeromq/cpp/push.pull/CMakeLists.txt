cmake_minimum_required(VERSION 3.22.2)

project(zmq.push.pull)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories (
  /opt/homebrew/include
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories(
  /opt/homebrew/lib
)

add_executable(push push.cpp)
add_executable(pull pull.cpp)

target_link_libraries(push zmq)
target_link_libraries(pull zmq)
