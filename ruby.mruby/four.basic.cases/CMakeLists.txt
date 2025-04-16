cmake_minimum_required (VERSION 3.30.0)

project (steps)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  step4.cpp
)

include_directories (
  /usr/local/include 
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib 
  /opt/homebrew/lib
)

add_executable(step1 step1.cpp)
add_executable(step2 step2.cpp)
add_executable(step3 step3.cpp)
add_executable(step4 step4.cpp)

target_link_libraries(step1 mruby)
target_link_libraries(step2 mruby)
target_link_libraries(step3 mruby)
target_link_libraries(step4 mruby)

