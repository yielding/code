cmake_minimum_required (VERSION 3.30.0)

project (md.analyzer)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  md.analyzer.cpp
  data_store_ext.cpp
  data_store.cpp
  file_system_ext.cpp
  file_system.cpp
  file_ext.cpp
  file.cpp
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

add_executable(md.analyzer 
  ${SOURCES}
)

target_link_libraries(md.analyzer 
  mruby
)
