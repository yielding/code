# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a C++26 demonstration project showcasing advanced error handling using `std::expected` with domain-specific error types and functional programming patterns. The project demonstrates how to integrate error codes across different layers (network, database) with proper error propagation and user-friendly error messaging.

## Build Commands

```bash
# Standard CMake build process
mkdir build && cd build
cmake .. && cmake --build .
./demo
```

## Architecture

### Core Components

- **expected_util.hpp** - Functional programming utilities for `std::expected`:
  - Pipe operator (`|`) for chaining operations
  - Adapter functions: `and_then_f`, `transform_f`, `transform_error_f`
  - Tee functions: `tee_value_f`, `tee_error_f`, `tee_both_f` for side effects without consuming the expected

- **domain_error.hpp** - Central error type with rich context:
  - `DomainError` struct with error code, source, operation context, and trace information
  - `Ex<T>` alias for `expected<T, DomainError>`
  - `Err()` helper for creating domain errors with context

- **Error Categories**:
  - **net_error.hpp** - Network-specific error codes (Timeout, ConnRefused, DnsFail, Proto)
  - **db_error.hpp** - Database-specific error codes (Deadlock, Constraint, Syntax)

- **service.hpp** - Business logic layer demonstrating error boundary mapping:
  - `http_get()` - Simulates network operations with various error scenarios
  - `db_query_user()` - Simulates database operations with error conditions  
  - `get_user_id()` - Composed operation showing error propagation patterns

### Key Patterns

- **Error Propagation**: Uses `std::expected` for composable error handling without exceptions
- **Domain Error Mapping**: Low-level system errors are mapped to domain-specific errors with context
- **Functional Composition**: Pipe operator enables clean chaining of operations
- **Error Context**: Rich error information includes operation, location, and trace data
- **User-Friendly Errors**: Internal technical errors are mapped to localized user messages

The demo showcases both successful operations and various error scenarios (network timeout, protocol errors, database deadlocks) with proper error context preservation.