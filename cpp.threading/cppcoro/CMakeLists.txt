cmake_minimum_required (VERSION 3.30.0)

project (main)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})

set (SOURCES
  main.cpp
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
  
  # macOS SDK paths (if needed)
  # execute_process(COMMAND xcrun --show-sdk-path OUTPUT_VARIABLE MACOS_SDK_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
  # include_directories(${MACOS_SDK_PATH}/usr/include)
  
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

# Common paths for all platforms
include_directories (
  ${HOME}/develop/include
  ${HOME}/develop/vendor/include
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  ${HOME}/develop/lib
)

# Optional: Find packages (recommended approach)
# find_package(Boost COMPONENTS system filesystem thread)
# if (Boost_FOUND)
#   include_directories(${Boost_INCLUDE_DIRS})
#   link_directories(${Boost_LIBRARY_DIRS})
# endif()

add_executable (${CMAKE_PROJECT_NAME}
  ${SOURCES}
)

# Optional: Link libraries
# target_link_libraries(${CMAKE_PROJECT_NAME}
#   ${Boost_LIBRARIES}
#   pthread
# )

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