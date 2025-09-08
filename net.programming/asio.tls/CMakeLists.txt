cmake_minimum_required (VERSION 3.30.0)

project (tls)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set (HOME $ENV{HOME})


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
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  ${HOME}/develop/lib
)

add_executable (client client.cpp)
add_executable (server server.cpp)


target_link_libraries(client
  PRIVATE
  ssl crypto
)


target_link_libraries(server
  PRIVATE
  ssl crypto
)
