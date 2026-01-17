# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the official gRPC C++ Hello World example demonstrating various RPC communication patterns. It implements a "Greeter" service with synchronous, asynchronous, and callback-based client/server pairs.

## Build Commands

### CMake (Recommended)
```bash
mkdir -p build && cd build
cmake ..
make
```

### Make (Legacy)
```bash
make all              # Build all binaries
make greeter_server   # Build specific target
make clean            # Clean artifacts
```

Prerequisites: gRPC and protobuf must be installed via cmake or system package manager. The Makefile uses pkg-config for dependency detection.

### Superbuild (All Dependencies from Source)
```bash
mkdir -p cmake_externalproject/build && cd cmake_externalproject/build
cmake ..
make
```

## Running the Examples

Start a server in one terminal, then run the corresponding client:
```bash
./greeter_server &
./greeter_client
```

## Architecture

### Service Definition (`helloworld.proto`)
Defines three RPC methods:
- `SayHello` - Unary RPC
- `SayHelloStreamReply` - Server streaming
- `SayHelloBidiStream` - Bidirectional streaming

### Implementation Patterns
| Pattern | Client | Server |
|---------|--------|--------|
| Synchronous | `greeter_client.cc` | `greeter_server.cc` |
| Callback | `greeter_callback_client.cc` | `greeter_callback_server.cc` |
| Async (CompletionQueue) | `greeter_async_client.cc`, `greeter_async_client2.cc` | `greeter_async_server.cc` |
| XDS-enabled | `xds_greeter_client.cc` | `xds_greeter_server.cc` |

### Key gRPC Concepts
- **Channel/Stub**: Clients create a `Channel` to the server, then use a `Stub` to make RPC calls
- **Service Implementation**: Servers inherit from `Greeter::Service` and override RPC methods
- **Context**: `ClientContext`/`ServerContext` carry metadata and control RPC behavior
- **CompletionQueue**: Used in async implementations for event-driven I/O

### Dependencies
- grpc++ / grpc++_reflection
- protobuf
- absl (flags, logging)
