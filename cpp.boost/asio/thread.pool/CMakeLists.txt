CMAKE_MINIMUM_REQUIRED (VERSION 3.15)

PROJECT (thread.pool)

SET (CMAKE_CXX_STANDARD 20)
SET (CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET (SOURCES
  thread.pool.cpp
  /Users/yielding/develop/src/file_util.cpp
)

INCLUDE_DIRECTORIES (
  /usr/local/include 
  ${CMAKE_CURRENT_SOURCE_DIR}
  /Users/yielding/develop/include
)

LINK_DIRECTORIES (
  /usr/local/lib
)

LINK_LIBRARIES (
  zmq
)

ADD_EXECUTABLE (thread.pool ${SOURCES})
