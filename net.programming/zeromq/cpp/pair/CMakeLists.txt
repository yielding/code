cmake_minimum_required(VERSION 3.30.0)

project(zmq.pair)

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

add_executable(cli pair.cli.cpp)
add_executable(srv pair.srv.cpp)

target_link_libraries(cli zmq)
target_link_libraries(srv zmq)

