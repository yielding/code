cmake_minimum_required(VERSION 3.10)

project(tcp_pipeline_boostasio)

set (CMAKE_CXX_STANDARD 26)

find_package(Boost REQUIRED COMPONENTS system)

add_executable(server server.cpp)
add_executable(client client.cpp)

target_link_libraries(server PRIVATE Boost::system pthread)
target_link_libraries(client PRIVATE Boost::system pthread)