cmake_minimum_required(VERSION 3.22.2)

project(zmq.req.rep)

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

add_executable(req ch1.cli.cpp)
add_executable(rep ch1.srv.cpp)

target_link_libraries(rep zmq)
target_link_libraries(req zmq)

