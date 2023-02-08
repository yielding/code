cmake_minimum_required (VERSION 3.22.2)

project (books)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  books.cpp
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR}
  ~/code/develop/include
  ~/code/develop/vendor/include)

ADD_EXECUTABLE(books ${SOURCES})
