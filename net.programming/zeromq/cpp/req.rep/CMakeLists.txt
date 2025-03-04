cmake_minimum_required(VERSION 3.30.0)

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

add_executable(req ch1.req.cpp)
add_executable(rep ch1.rep.cpp)

target_link_libraries(rep zmq)
target_link_libraries(req zmq)
