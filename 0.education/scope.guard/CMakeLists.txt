cmake_minimum_required (VERSION 4.0.0)

project (main LANGUAGES CXX)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  scope.guard.cpp
)

# Platform detection
if (APPLE)
  # macOS specific paths (Apple Silicon)
  set (HOMEBREW_PREFIX "/opt/homebrew")

  if (EXISTS ${HOMEBREW_PREFIX})
    include_directories (
      ${HOMEBREW_PREFIX}/include
    )
    link_directories (
      ${HOMEBREW_PREFIX}/lib
    )
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

include_directories (
  ${CMAKE_CURRENT_SOURCE_DIR}
)

list (APPEND CMAKE_PREFIX_PATH "$ENV{HOME}/.local")
find_package (cpp.common CONFIG QUIET)
if (cpp.common_FOUND)
  message (STATUS "cpp.common: find_package ${cpp.common_VERSION} (${cpp.common_DIR})")
  set (CPP_COMMON_LIB cpp.common::core)
else ()
  message (STATUS "cpp.common: not installed — using ~/develop source tree")
  include_directories (
    ${HOME}/develop/include
    ${HOME}/develop/vendor/include
  )
  link_directories (${HOME}/develop/lib)
  if (EXISTS ${HOME}/develop/lib/libcore.a)
    set (CPP_COMMON_LIB core)
  else ()
    set (CPP_COMMON_LIB "")
  endif ()
endif ()

add_executable (${CMAKE_PROJECT_NAME}
  ${SOURCES}
)

target_link_libraries (${CMAKE_PROJECT_NAME} PRIVATE
  ${CPP_COMMON_LIB}
)

# Compiler flags
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE
    -freflection   # C++26 static reflection (P2996) — REQUIRED for ^^ / [: :] / template for
    -Wall
    -Wextra
    -Wpedantic
    # -O2  # Optimization
    # -g   # Debug symbols
  )
endif()
