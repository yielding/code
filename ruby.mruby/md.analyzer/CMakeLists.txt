cmake_minimum_required (VERSION 3.30.0)

project (md.analyzer)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

# libmruby.a is built without -fPIC; PIE link fails on Linux
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_link_options (-no-pie)
endif()

set (SOURCES
  main.cpp
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
  $ENV{HOME}/.rubies/mruby-3.4.0/include
  $ENV{HOME}/.rubies/mruby-3.4.0/build/host/include
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../mrubybind
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
  $ENV{HOME}/.rubies/mruby-3.4.0/build/host/lib
)

add_executable(md.analyzer 
  ${SOURCES}
)

target_link_libraries(md.analyzer 
  mruby
)
