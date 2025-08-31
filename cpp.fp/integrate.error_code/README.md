# expected_util + DomainError Full Demo

## Build
```bash
mkdir build && cd build
cmake .. && cmake --build .
./demo
```

## Files
- `expected_util.hpp` — Pipe (`|`) + adapters and tee helpers
- `domain_error.hpp` — `DomainError` (code + source + context)
- `net_error.hpp` / `db_error.hpp` — subdomain error categories
- `service.hpp` — boundary mapping + business flow
- `demo.cpp` — usage with tee_both/tee_error and transform_f
