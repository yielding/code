cmake_minimum_required(VERSION 3.14)

project(ClientStreaming C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Threads REQUIRED)
find_package(absl CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)
find_package(gRPC CONFIG REQUIRED)

find_program(PROTOC protoc REQUIRED)
find_program(GRPC_CPP_PLUGIN grpc_cpp_plugin REQUIRED)

# Proto file
set(proto_file "${CMAKE_CURRENT_SOURCE_DIR}/sensor.proto")

# Generated sources
set(proto_srcs "${CMAKE_CURRENT_BINARY_DIR}/sensor.pb.cc")
set(proto_hdrs "${CMAKE_CURRENT_BINARY_DIR}/sensor.pb.h")
set(grpc_srcs  "${CMAKE_CURRENT_BINARY_DIR}/sensor.grpc.pb.cc")
set(grpc_hdrs  "${CMAKE_CURRENT_BINARY_DIR}/sensor.grpc.pb.h")

add_custom_command(
  OUTPUT "${proto_srcs}" "${proto_hdrs}" "${grpc_srcs}" "${grpc_hdrs}"
  COMMAND ${PROTOC}
  ARGS --grpc_out "${CMAKE_CURRENT_BINARY_DIR}"
       --cpp_out "${CMAKE_CURRENT_BINARY_DIR}"
       -I "${CMAKE_CURRENT_SOURCE_DIR}"
       --plugin=protoc-gen-grpc="${GRPC_CPP_PLUGIN}"
       "${proto_file}"
  DEPENDS "${proto_file}"
  COMMENT "Generating protobuf and gRPC sources for sensor.proto"
)

include_directories("${CMAKE_CURRENT_BINARY_DIR}")

add_library(sensor_grpc_proto
  ${grpc_srcs}
  ${grpc_hdrs}
  ${proto_srcs}
  ${proto_hdrs}
)

target_link_libraries(sensor_grpc_proto
  PUBLIC
    protobuf::libprotobuf
    gRPC::grpc++
    gRPC::grpc++_reflection
)

set(ALL_TARGETS sensor_server sensor_client)

foreach(_target ${ALL_TARGETS})
  add_executable(${_target} "${_target}.cpp")
  target_link_libraries(${_target}
    sensor_grpc_proto
    absl::flags
    absl::flags_parse
    absl::log
    absl::check
    Threads::Threads
  )
endforeach()
