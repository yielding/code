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
make client

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

**client.c**: Test client implementation  
- Sends test image data (640x480x3 BGR8 format)
- JSON header with metadata (op, mime type, dimensions, pixel format)
- Reads and displays server response

## Usage Pattern

```bash
# Run server and client with pipes
./client | ./server | tee response.bin

# Or use with C# ProcessRedirect (parent directory)
# Server can be spawned as subprocess with redirected stdio
```

## Development Notes

- C11 standard required
- Memory management: Dynamic allocation for headers/payloads, proper cleanup on all paths
- Error handling: perror for diagnostics, early exit on I/O failures
- Platform compatibility: Tested on Windows, macOS, Linux