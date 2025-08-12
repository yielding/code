# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Boost.Beast-based file upload project for handling HTTP file uploads in C++. The project uses modern C++ (C++26) and CMake build system.

## Build Commands

### Build with CMake
```bash
# Create build directory and configure
mkdir -p build && cd build
cmake ..

# Build the project
make

# Run the executable
./main
```

### Clean build
```bash
rm -rf build/
```

## Project Structure

- `main.cpp` - Main application entry point
- `CMakeLists.txt` - CMake build configuration
- C++26 standard with modern features (`<print>`)

## Dependencies

- C++26 compiler (Clang or GCC with C++26 support)
- CMake 3.30.0 or higher
- Boost libraries (currently commented out in CMakeLists.txt, uncomment when implementing Beast functionality)

## Development Notes

- The project is configured for both macOS (with Homebrew) and Linux platforms
- Homebrew paths are automatically detected on macOS (`/opt/homebrew`)
- Custom include/library paths from `~/develop/` are included
- Compiler warnings are enabled (`-Wall -Wextra -Wpedantic`)

## Next Steps for Implementation

When implementing the Beast file upload functionality:
1. Uncomment the Boost package finding and linking in CMakeLists.txt
2. Add necessary Boost.Beast components (system, filesystem, thread)
3. Implement HTTP server with file upload endpoint
4. Handle multipart/form-data parsing for file uploads