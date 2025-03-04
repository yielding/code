cmake_minimum_required(VERSION 3.30.0)

project(zmq.pub.sub)

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

add_executable(pub pub.cpp)
add_executable(sub sub.cpp)
add_executable(sub.last sub.last.cpp)

target_link_libraries(pub zmq)
target_link_libraries(sub zmq)
target_link_libraries(sub.last zmq)

