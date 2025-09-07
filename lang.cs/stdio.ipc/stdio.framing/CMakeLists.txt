cmake_minimum_required(VERSION 3.30.0)

project(xplat_stdio_framing)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(HOME $ENV{HOME})

# Modern C++ executables
add_executable(server server.cpp)
add_executable(improved_client improved_client.cpp)
add_executable(pipe_client pipe_client.cpp)

# Include directories
target_include_directories(server PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(improved_client PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(pipe_client PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Platform-specific settings
if (WIN32)
  target_compile_definitions(server PRIVATE _CRT_SECURE_NO_WARNINGS)
  target_compile_definitions(improved_client PRIVATE _CRT_SECURE_NO_WARNINGS)
  target_compile_definitions(pipe_client PRIVATE _CRT_SECURE_NO_WARNINGS)
endif()
