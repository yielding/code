cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})
set (ORT_ROOT $ENV{ORT_ROOT})

set (SOURCES
  main.cpp
)

# Platform detection
if (APPLE)
  # macOS: use Homebrew onnxruntime
  set (HOMEBREW_PREFIX "/opt/homebrew")

  if (EXISTS ${HOMEBREW_PREFIX}/include/onnxruntime)
    set (ORT_INCLUDE_DIR ${HOMEBREW_PREFIX}/include/onnxruntime)
    set (ORT_LIB_DIR ${HOMEBREW_PREFIX}/lib)
  elseif (ORT_ROOT)
    set (ORT_INCLUDE_DIR ${ORT_ROOT}/include)
    set (ORT_LIB_DIR ${ORT_ROOT}/lib)
  endif()

  include_directories (
    ${HOMEBREW_PREFIX}/include
  )
  link_directories (
    ${HOMEBREW_PREFIX}/lib
  )

elseif (UNIX AND NOT APPLE)
  # Linux: use ORT_ROOT or system paths
  if (ORT_ROOT)
    set (ORT_INCLUDE_DIR ${ORT_ROOT}/include)
    set (ORT_LIB_DIR ${ORT_ROOT}/lib)
  else()
    set (ORT_INCLUDE_DIR /usr/local/include)
    set (ORT_LIB_DIR /usr/local/lib)
  endif()

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

# Common paths for all platforms
include_directories (
  ${HOME}/develop/include
  ${ORT_INCLUDE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  ${HOME}/develop/lib
)

add_executable (${CMAKE_PROJECT_NAME}
  ${SOURCES}
)

target_link_libraries(${CMAKE_PROJECT_NAME}
  onnxruntime
)

target_link_directories(${CMAKE_PROJECT_NAME} PRIVATE
  ${ORT_LIB_DIR}
)

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