cmake_minimum_required(VERSION 3.22.2)

project(zmq.dealer.router)

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

add_executable(dealer dealer.cpp)
add_executable(router router.cpp)

target_link_libraries(dealer zmq)
target_link_libraries(srv zmq)
