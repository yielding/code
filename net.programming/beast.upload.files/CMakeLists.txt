cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  main.cpp
  upload_client.cpp
)

if (APPLE)
  set (HOMEBREW_PREFIX "/opt/homebrew")
  
  if (EXISTS ${HOMEBREW_PREFIX})
    include_directories ( ${HOMEBREW_PREFIX}/include )
    link_directories ( ${HOMEBREW_PREFIX}/lib )
  endif()
elseif (UNIX AND NOT APPLE)
  # Linux specific paths
  include_directories (
    /usr/local/include
    /usr/include
  )
  link_directories (
    /usr/local/lib
    /usr/lib
    /usr/lib/x86_64-linux-gnu  # Debian/Ubuntu
  )
endif()

# Find packages
find_package(Boost REQUIRED COMPONENTS filesystem thread serialization)
find_package(nlohmann_json REQUIRED)

# Create executable
add_executable (${CMAKE_PROJECT_NAME}
  ${SOURCES}
)

# Add include directories and link libraries to the target
if (Boost_FOUND)
  target_include_directories(${CMAKE_PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS})
  target_link_directories(${CMAKE_PROJECT_NAME} PRIVATE ${Boost_LIBRARY_DIRS})
  target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE ${Boost_LIBRARIES})
endif()

# Link nlohmann_json
target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE nlohmann_json::nlohmann_json)

# Optional: Compiler flags
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    # -O2  # Optimization
    # -g   # Debug symbols
  )
endif()