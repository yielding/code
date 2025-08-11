cmake_minimum_required(VERSION 3.30.0)

project(xplat_stdio_framing)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(HOME $ENV{HOME})


# Modern C++ executables
add_executable(server server.cpp)
add_executable(client client.cpp)

# Include directories
target_include_directories(server PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(client PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Platform-specific settings
if (WIN32)
  target_compile_definitions(server PRIVATE _CRT_SECURE_NO_WARNINGS)
  target_compile_definitions(client PRIVATE _CRT_SECURE_NO_WARNINGS)
endif()
