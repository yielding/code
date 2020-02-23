CMAKE_MINIMUM_REQUIRED (VERSION 3.15)

PROJECT (mat.serialize)

SET (CMAKE_CXX_STANDARD 20)
SET (CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET (SOURCES
  mat.serialize.cpp
)

FIND_PACKAGE (OpenCV REQUIRED)
FIND_PACKAGE (Boost 1.72 REQUIRED)

INCLUDE_DIRECTORIES (
  /usr/local/include
  ${Boost_INCLUDE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
)

LINK_DIRECTORIES(
  /usr/local/lib
  ${Boost_LIBRARY_DIR}
)

ADD_EXECUTABLE (mat.serialize ${SOURCES})

TARGET_LINK_LIBRARIES (mat.serialize
  ${OpenCV_LIBS}
  boost_serialization
  boost_iostreams
)
