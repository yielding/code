# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal coding practice repository containing algorithm implementations, language explorations, and programming exercises across multiple languages with a primary focus on modern C++.

## Build Commands

### Primary Build System (Rake)
```bash
rake osx          # Build for macOS (default)
rake test         # Build and run tests
rake run          # Build and execute
rake clean        # Clean build artifacts
```

Build configuration: `build/app2.rake`
- Uses C++23/C++2c standard
- Supports g++, clang++, xcrun compilers
- Boost libraries integrated

### Secondary Build Systems
- CMake: Used in specific subdirectories
- Make: Legacy projects
- Jamroot: Boost.Build for Boost-specific projects

## Architecture & Structure

### Major Directories
- `0.algorithm/` - Algorithm practice, Project Euler solutions, data structures
- `cpp.*/` - C++ feature exploration organized by topic:
  - `cpp.boost/` - Boost library usage examples
  - `cpp.lang/` - Modern C++ language features
  - `cpp.fp/` - Functional programming in C++
  - `cpp.std.lib/` - Standard library exploration
  - `cpp.threading/` - Concurrency and parallelism
- `lang.*/` - Other language implementations (Ruby, Python, Haskell, Swift, etc.)
- `ai/` - Machine learning experiments (PyTorch)
- `parallel/` - Parallel computing (OpenCL)

### Key Technologies
- **C++**: C++23/C++2c, Boost libraries, Range-v3, template metaprogramming
- **Ruby**: Algorithm implementations, build scripts (Rake)
- **Python**: ML experiments, data processing
- **Testing**: Google Test for C++

## Development Notes

- This is a learning/experimental repository - code quality varies by project
- Heavy use of C++ template metaprogramming and functional programming concepts
- Multiple build systems present - check local directory for specific build method
- When working with C++ code, assume modern standards (C++23/C++2c) unless specified
- Boost libraries are available and commonly used throughout C++ projects

## Coding Style Guidelines

This project follows a consistent C++ coding style. All new code should adhere to these conventions:

### File Structure
- **Header guard**: Use `#pragma once`
- **Include order**: 
  1. Project headers (e.g., `#include "header.hpp"`)
  2. Empty line
  3. Standard library headers (e.g., `#include <string>`)
- **Section separators**: Use comment blocks for major sections:
  ```cpp
  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  ```

### Namespace and Using Declarations
- Place `using namespace std;` inside the namespace (not globally)
- In implementation files, also add other using declarations as needed

### Class Design
- **Member organization**:
  1. `public:` constructors/destructors
  2. `public:` methods
  3. `private:` methods
  4. `private:` member variables
- **Member variables**: Prefix with underscore (e.g., `_variable_name`)
- **Method style**: Use trailing return type syntax:
  ```cpp
  auto function_name() -> return_type
  ```

### Formatting
- **Constructor initialization lists**: Colon on same line, comma at start of each subsequent line:
  ```cpp
  Constructor(params)
    : _member1(value1)
    , _member2(value2)
  {
  }
  ```
- **Type usage**: Use types directly without `std::` prefix within namespace:
  ```cpp
  string not std::string
  expected<T, E> not std::expected<T, E>
  ```
- **Type deduction**: Use CTAD (Class Template Argument Deduction) where possible:
  ```cpp
  atomic g_shutdown{false};  // not atomic<bool>
  vector v{1, 2, 3};         // not vector<int>
  ```
- **Function parameters**: Always use `const` for value parameters to prevent modification:
  ```cpp
  void function(const int value)     // not void function(int value)
  auto process(const string name)    // not auto process(string name)
  ```

### Example Header File
```cpp
#pragma once

#include "base_class.hpp"

#include <string>
#include <memory>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace my_namespace
{
  using namespace std;

  class ExampleClass
  {
  public:
    ExampleClass(const string name);
    ~ExampleClass();

  public:
    auto process() -> bool;
    auto get_name() const -> string;

  private:
    auto internal_helper() -> void;

  private:
    string _name;
    int _count;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
```