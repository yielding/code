cmake_minimum_required(VERSION 3.28)

project (calc)

set (CMAKE_CXX_STANDARD 23)
set (CMAKE_CXX_COMPILER g++-14)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (SOURCES
  main.cpp
  ExprBaseListener.cpp
  CalcInterpreter.cpp
  ExprListener.cpp
  ExprLexer.cpp
  Exprparser.cpp
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
