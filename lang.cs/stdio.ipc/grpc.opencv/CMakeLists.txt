cmake_minimum_required(VERSION 3.30.0)

project(grpc_opencv)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

################################################################################
#
# Set paths - prefer Homebrew installation
#
################################################################################

list(PREPEND CMAKE_PREFIX_PATH "/opt/homebrew/lib/cmake")

################################################################################
#
# Find required packages
#
################################################################################

find_package(OpenCV REQUIRED)
find_package(Protobuf CONFIG REQUIRED)
find_package(gRPC CONFIG REQUIRED)
find_package(absl CONFIG REQUIRED)

# Find grpc_cpp_plugin and protoc from Homebrew
find_program(GRPC_CPP_PLUGIN grpc_cpp_plugin PATHS "/opt/homebrew/bin" NO_DEFAULT_PATH)
if(NOT GRPC_CPP_PLUGIN)
  find_program(GRPC_CPP_PLUGIN grpc_cpp_plugin)
endif()

find_program(PROTOC_EXECUTABLE protoc PATHS "/opt/homebrew/bin" NO_DEFAULT_PATH)
if(NOT PROTOC_EXECUTABLE)
  find_program(PROTOC_EXECUTABLE protoc)
endif()

message(STATUS "Using grpc_cpp_plugin: ${GRPC_CPP_PLUGIN}")
message(STATUS "Using protoc: ${PROTOC_EXECUTABLE}")

################################################################################
#
# Generate protobuf and gRPC sources
#
################################################################################

set(PROTO_FILE "${CMAKE_CURRENT_SOURCE_DIR}/image_processor.proto")
set(PROTO_SRC_DIR "${CMAKE_CURRENT_BINARY_DIR}")

# Generate protobuf sources
set(PROTO_SRCS "${PROTO_SRC_DIR}/image_processor.pb.cc")
set(PROTO_HDRS "${PROTO_SRC_DIR}/image_processor.pb.h")
set(GRPC_SRCS "${PROTO_SRC_DIR}/image_processor.grpc.pb.cc")
set(GRPC_HDRS "${PROTO_SRC_DIR}/image_processor.grpc.pb.h")

add_custom_command(
  OUTPUT ${PROTO_SRCS} ${PROTO_HDRS} ${GRPC_SRCS} ${GRPC_HDRS}
  COMMAND ${PROTOC_EXECUTABLE}
  ARGS --grpc_out=${PROTO_SRC_DIR}
       --cpp_out=${PROTO_SRC_DIR}
       --plugin=protoc-gen-grpc=${GRPC_CPP_PLUGIN}
       -I${CMAKE_CURRENT_SOURCE_DIR}
       ${PROTO_FILE}
  DEPENDS ${PROTO_FILE}
  COMMENT "Generating protobuf and gRPC sources"
)

################################################################################
#
# Include directories
#
################################################################################

include_directories(
  ${PROTO_SRC_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  /opt/homebrew/include
)

link_directories(
  /opt/homebrew/lib
)

################################################################################
#
# Proto library
#
################################################################################

add_library(proto_lib
  ${PROTO_SRCS}
  ${GRPC_SRCS}
)

target_link_libraries(proto_lib
  gRPC::grpc++
  protobuf::libprotobuf
)

################################################################################
#
# Server executable
#
################################################################################

add_executable(server
  server.cpp
)

target_link_libraries(server
  proto_lib
  ${OpenCV_LIBS}
  gRPC::grpc++
  protobuf::libprotobuf
)

################################################################################
#
# Client executable
#
################################################################################

add_executable(client
  client.cpp
)

target_link_libraries(client
  proto_lib
  gRPC::grpc++
  protobuf::libprotobuf
)
