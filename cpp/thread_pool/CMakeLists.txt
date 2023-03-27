cmake_minimum_required (VERSION 3.26)

project (thread.pool)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
)

include_directories (
  /opt/homebrew/include
  ~/develop/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

ADD_EXECUTABLE (thread.pool ${SOURCES})
target_link_libraries (thread.pool
  boost_thread-mt
  boost_system-mt
)
