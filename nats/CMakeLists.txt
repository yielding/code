cmake_minimum_required(VERSION 3.20)

project(nats_demo CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Threads REQUIRED)

# ------------------------------------------------------------------------------
# cnats(nats.c) 준비
#   1) 시스템에 설치되어 있으면 그것을 쓴다 (apt install libnats-dev / make install)
#   2) 없으면 github 에서 소스를 받아 static 으로 함께 빌드한다
# ------------------------------------------------------------------------------
set(NATS_C_TAG "v3.9.3" CACHE STRING "nats.c tag used when fetching from source")

find_package(cnats CONFIG QUIET)

if(TARGET cnats::nats)
  set(NATS_TARGET cnats::nats)
  message(STATUS "using installed cnats (shared)")
elseif(TARGET cnats::nats_static)
  set(NATS_TARGET cnats::nats_static)
  message(STATUS "using installed cnats (static)")
else()
  message(STATUS "cnats not found - fetching nats.c ${NATS_C_TAG}")

  set(NATS_BUILD_STREAMING  OFF CACHE BOOL "" FORCE)  # 폐기된 NATS Streaming 은 제외
  set(NATS_BUILD_EXAMPLES   OFF CACHE BOOL "" FORCE)
  set(NATS_BUILD_LIB_SHARED OFF CACHE BOOL "" FORCE)
  set(NATS_BUILD_LIB_STATIC ON  CACHE BOOL "" FORCE)
  set(BUILD_TESTING         OFF CACHE BOOL "" FORCE)

  include(FetchContent)
  FetchContent_Declare(cnats
    GIT_REPOSITORY https://github.com/nats-io/nats.c.git
    GIT_TAG        ${NATS_C_TAG}
    GIT_SHALLOW    TRUE)
  FetchContent_MakeAvailable(cnats)

  set(NATS_TARGET nats_static)
endif()

# ------------------------------------------------------------------------------
# 공통 래퍼
# ------------------------------------------------------------------------------
add_library(nats_util STATIC nats_util.cpp)
target_include_directories(nats_util PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(nats_util PUBLIC ${NATS_TARGET} Threads::Threads)

# ------------------------------------------------------------------------------
# 예제
# ------------------------------------------------------------------------------
set(ALL_TARGETS core_server core_client js_server js_client)

foreach(_target ${ALL_TARGETS})
  add_executable(${_target} "${_target}.cpp")
  target_link_libraries(${_target} PRIVATE nats_util)
endforeach()
