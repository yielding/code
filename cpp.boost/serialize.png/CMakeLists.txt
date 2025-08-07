cmake_minimum_required (VERSION 3.30.0)

project (driver)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  driver.cpp
)

find_package(OpenCV REQUIRED)

include_directories (
  ${HOME}/develop/include
  /opt/homebrew/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (driver
  ${SOURCES}
)

target_link_libraries (driver
  ${OpenCV_LIBS}
)

