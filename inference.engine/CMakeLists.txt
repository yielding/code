cmake_minimum_required (VERSION 3.16)

project (uie_client)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find packages
find_package (Protobuf REQUIRED CONFIG)
find_package (gRPC REQUIRED CONFIG)

# Proto files
set (PROTO_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set (GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)
file (MAKE_DIRECTORY ${GENERATED_DIR})

# Find protoc and grpc_cpp_plugin
find_program (PROTOC protoc)
find_program (GRPC_CPP_PLUGIN grpc_cpp_plugin)

# Proto source files
set (PROTO_FILES
  ${PROTO_DIR}/common.proto
  ${PROTO_DIR}/operations.proto
  ${PROTO_DIR}/object_analysis.proto
)

# Generate protobuf and gRPC sources
set (PROTO_SRCS)
set (PROTO_HDRS)
set (GRPC_SRCS)
set (GRPC_HDRS)

foreach (PROTO_FILE ${PROTO_FILES})
  get_filename_component (PROTO_NAME ${PROTO_FILE} NAME_WE)

  set (PB_SRC  ${GENERATED_DIR}/${PROTO_NAME}.pb.cc)
  set (PB_HDR  ${GENERATED_DIR}/${PROTO_NAME}.pb.h)
  set (GRPC_SRC ${GENERATED_DIR}/${PROTO_NAME}.grpc.pb.cc)
  set (GRPC_HDR ${GENERATED_DIR}/${PROTO_NAME}.grpc.pb.h)

  add_custom_command (
    OUTPUT ${PB_SRC} ${PB_HDR} ${GRPC_SRC} ${GRPC_HDR}
    COMMAND ${PROTOC}
      --proto_path=${PROTO_DIR}
      --cpp_out=${GENERATED_DIR}
      --grpc_out=${GENERATED_DIR}
      --plugin=protoc-gen-grpc=${GRPC_CPP_PLUGIN}
      ${PROTO_FILE}
    DEPENDS ${PROTO_FILE}
    COMMENT "Generating protobuf/gRPC for ${PROTO_NAME}.proto"
  )

  list (APPEND PROTO_SRCS ${PB_SRC})
  list (APPEND PROTO_HDRS ${PB_HDR})
  list (APPEND GRPC_SRCS  ${GRPC_SRC})
  list (APPEND GRPC_HDRS  ${GRPC_HDR})
endforeach()

# Proto library
add_library (uie_proto
  ${PROTO_SRCS}
  ${GRPC_SRCS}
)

target_include_directories (uie_proto PUBLIC
  ${GENERATED_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries (uie_proto PUBLIC
  gRPC::grpc++
  protobuf::libprotobuf
)

# Main executable
add_executable (${CMAKE_PROJECT_NAME}
  main.cpp
)

target_include_directories (${CMAKE_PROJECT_NAME} PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries (${CMAKE_PROJECT_NAME} PRIVATE
  uie_proto
)
