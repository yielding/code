cmake_minimum_required (VERSION 3.22.0)
project (tutorial1)

set (TUTORIAL_VERSION_MAJOR 1)
set (TUTORIAL_VERSION_MINOR 0)

set (CMAKE_CXX_STANDARD 23)

option (USE_MYPATH "Use tutorial provided math impl" ON)

configure_file (
  "${PROJECT_SOURCE_DIR}/tutorial_config.h.in"
  "${PROJECT_BINARY_DIR}/tutorial_config.h"
)

if (USE_MYPATH)
  include_directories ("${PROJECT_SOURCE_DIR}/my_math")
  add_subdirectory (my_math)
  set (EXTRA_LIBS ${EXTRA_LIBS} math_functions)
endif()

include_directories("${PROJECT_BINARY_DIR}")

add_executable(tutorial1 tutorial1.cpp)
target_link_libraries (tutorial1 ${EXTRA_LIBS})
