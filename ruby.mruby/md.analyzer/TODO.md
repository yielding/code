# REMARK
  - The items "DONE" are all the elements that will be used for script embedding.\
    completed well.

# TODO
  + real filesystem backend (replace the toy data with actual parsing)
  + richer MD::File I/O (read/seek against real data, enumerate files per filesystem)
  + expose more domain objects under the MD module as the product grows

# DOING

# DONE
  + mruby basic embedding
    - global variable
    - access array of user-defined object using block
      : JSON example of ArangoDB
    - hash of my object
    - return fixnum
    - return float
    - block

  + filesystem toy
    - filesystem
    - file

  + refactoring
    - mrubybind helper: ownership-aware (owned/borrowed holder) DATA binding,
      type-checked args from C++ signatures, arena_guard, vm with context-based
      loading and error reporting
    - split FileSystem ownership: borrowed from DataStore vs GC-owned from Ruby
    - single main.cpp built by both CMake and Rake
    - C++ classes renamed to snake_case: data_store, file_system, file_base, file
    - script classes namespaced under MD (MD::DataStore, MD::Filesystem, MD::File)
      : builtin ::File from mruby-io stays untouched
    - boost::format replaced with C++26 std::format (no boost dependency)
    - default compiler g++-16 for both CMake and Rake
