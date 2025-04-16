cmake_minimum_required (VERSION 3.30.0)

project (calc)

set (CMAKE_CXX_STANDARD 26)
set (CMAKE_CXX_COMPILER clang++)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  ./main.cpp
  ./LabeledExprLexer.cpp
  ./LabeledExprParser.cpp
  ./LabeledExprVisitor.cpp
  ./LabeledExprBaseVisitor.cpp
)

include_directories (
  /usr/local/include
  /opt/homebrew/include
  /opt/homebrew/include/antlr4-runtime
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/lib
  /opt/homebrew/lib
)

add_executable (calc
  ${SOURCES}
)

target_link_libraries (calc
  PUBLIC
  antlr4-runtime
)
