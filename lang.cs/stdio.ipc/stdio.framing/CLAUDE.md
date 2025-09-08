# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Cross-platform stdio-based IPC (Inter-Process Communication) implementation using message framing protocol in C. This project implements a client-server architecture that communicates via stdin/stdout with binary message framing, suitable for subprocess communication or piping between processes.

## Build Commands

```bash
# Create build directory and compile
mkdir -p build && cd build
cmake ..
make

# Build specific targets
make server
make improved_client
make pipe_client

# Clean build
rm -rf build
```

## Architecture

### Protocol Design
- **Message Format**: 4-byte header length (BE) + 8-byte payload length (BE) + JSON header + binary payload
- **Endianness**: Network byte order (big-endian) for all numeric fields
- **Transport**: stdin/stdout with binary mode enabled on Windows

### Key Components

**xplat_io.h**: Cross-platform I/O abstraction layer
- Platform-specific stdio binary mode handling (Windows vs POSIX)
- Byte order conversion functions (host_to_be32/64, be32/64_to_host)
- Reliable I/O functions (read_exact, write_all) with EINTR handling

**server.c**: Message processing server
- Reads framed messages from stdin
- Processes payload (currently echo/passthrough)
- Writes framed response to stdout
- Handles variable-length headers and payloads

**client.cpp**: Test client implementation  
- Sends test image data (640x480x3 BGR8 format)
- JSON header with metadata (op, mime type, dimensions, pixel format)
- Reads and displays server response

**improved_client.cpp**: Enhanced C++ client with modular design
- Uses modern C++23 features and design patterns
- Supports pluggable message generators and response handlers
- Template-based architecture for extensibility

**pipe_client.cpp**: Automated client with subprocess management
- Automatically starts server process and establishes pipe communication
- Cross-platform subprocess creation (Windows CreateProcess/POSIX fork+exec)
- Manages process lifecycle with proper cleanup and termination

## Usage Patterns

### Manual Pipe Connection
```bash
# Traditional pipe-based communication
./client | ./server | tee response.bin
./improved_client | ./server
```

### Automated Pipe Connection  
```bash
# Automated client that spawns server process
./pipe_client ./server

# Usage with full path
./pipe_client /path/to/server
```

### Integration with Other Languages
```bash
# Use with C# ProcessRedirect (parent directory)
# Server can be spawned as subprocess with redirected stdio
```

## Development Notes

### Language Standards
- **C components**: C11 standard required
- **C++ components**: C++23/C++26 standard with modern features

### Memory Management
- Dynamic allocation for headers/payloads with proper cleanup on all paths
- RAII-based resource management in C++ components
- Automatic subprocess lifecycle management in pipe_client

### Error Handling
- C components: perror for diagnostics, early exit on I/O failures  
- C++ components: std::expected<T, std::error_code> for error propagation
- Cross-platform error code mapping

### Platform Compatibility
- Tested on Windows, macOS, Linux
- Cross-platform I/O abstraction layer
- Platform-specific subprocess management (CreateProcess/fork+exec)