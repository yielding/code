# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Long-running experiments in embedding mruby into C++ host applications. The goal is
bidirectional value exchange between mruby scripts and the host to provide dynamic
extensibility for a product (a forensic analyzer). Each subdirectory is an independent,
self-contained experiment; `md.analyzer/` is the main product prototype.

The parent repo's `/home/yielding/code/CLAUDE.md` coding style rules apply here as well.

## Prerequisites

- mruby 3.4.0 installed at `~/.rubies/mruby-3.4.0` (headers under `include/` and
  `build/host/include`, library under `build/host/lib`). CMake and Rakefiles hard-code
  these paths.
- Boost (boost::format is used in `jukebox` and `md.analyzer`).

## Build Commands

Two build systems coexist; check the subdirectory.

### CMake (md.analyzer, four.basic.cases)
```bash
cd md.analyzer
cmake -B build/debug -S .
cmake --build build/debug
./build/debug/md.analyzer      # must run from md.analyzer/ — loads ./myscript.rb by cwd
```

### Rake (basic, jukebox, wiki, hello, dl, md.analyzer)
Rakefiles load the shared build script `~/code/build/app2.rake` (`:mvm` flags refer to
the mruby VM include/lib paths). They target macOS (`task :default => [:osx]`), so on
Linux prefer the CMake builds.

There are no automated tests. Verification is running the executable and reading stdout;
each executable loads its Ruby script (`myscript.rb`, `mytest.rb`, `wiki-example.rb`,
`step*.rb`) from the current working directory at runtime — always run from the
subdirectory containing the script.

## Directory Guide

- `four.basic.cases/` — the 4 canonical embedding steps: run a code string, run a file,
  call Ruby from C, call C from Ruby. Start here to understand the basics.
- `hello/`, `dl/` — minimal embedding; `dl` links mruby as a dylib.
- `basic/` — exposing a C++ class to mruby; uses the JSON mrbgem.
- `wiki/` — C++ creates/calls a Ruby class instance; catalog of `mrb_get_args` format
  specifiers (`i`, `S`, `z`, `s`, `A!`).
- `jukebox/` — Pickaxe CDJukeBox example ported to mruby: DATA-type wrapping, block
  yield from C, returning C++ collections as Ruby arrays.
- `md.analyzer/` — main prototype: exposes DataStore/FileSystem/CFile domain objects to
  scripts. `TODO.md` tracks progress; refactoring is the current phase.
- `ref.doc/if_mruby.c` — vim's mruby interface (reference material, not built).

## Binding Architecture (md.analyzer)

Each domain class comes in a pair:
- `foo.h/.cpp` — plain C++ domain class, no mruby knowledge.
- `foo_ext.h/.cpp` — mruby binding: a `mrb_data_type` with a free function, C callback
  functions, and an `init_foo(mrb_state*)` that defines the Ruby class
  (`MRB_SET_INSTANCE_TT(cls, MRB_TT_DATA)`) and its methods.

`main.cpp` (the single entry point, built by both the Rakefile and CMake) opens the VM,
calls each `init_*`, loads and runs `myscript.rb` (or the script given as `argv[1]`),
checks `mrb->exc`, closes the VM. The DataStore singleton is exposed to scripts as the
global `$ds` via `mrb_gv_set`.

## Ownership Conventions (critical when touching bindings)

Who deletes the wrapped pointer is per-class and inconsistent — check the free function
before changing anything:
- **Borrowed** (free fn only logs, C++ owns): DataStore singleton, FileSystem pointers
  handed out by `ds_get_file_systems`.
- **Owned** (free fn deletes, mruby GC owns): CFile, DVD, CDJukeBox.
- Collections are returned by copy: `jb_get_dvd_list` news a copy of each element and
  transfers ownership to the GC — the correct pattern for value-like objects.
- Known hazard: `FileSystem` uses one `mrb_data_type` for both borrowed (from DataStore)
  and Ruby-constructed (`FileSystem.new`) instances, so Ruby-created ones leak; do not
  add `delete` to `fs_free` without splitting the two cases.

## Embedding Gotchas Learned Here

- Re-initialization pattern: set `DATA_TYPE(self) = NULL` before `new`, then assign
  `DATA_PTR`/`DATA_TYPE` (see `fi_initialize` in `file_ext.cpp`; the safe ordering).
- Prefer `mrb_get_args` format specifiers (`"S"`, `"i"`, ...) over `mrb_get_arg1` +
  raw `RSTRING_PTR` — the specifier does type checking and raises TypeError instead of
  crashing the host.
- When building large Ruby arrays/hashes in a loop from C, wrap each iteration with
  `mrb_gc_arena_save`/`mrb_gc_arena_restore` after the element is anchored in the
  container.
- Parse with an `mrbc_context` (+ `mrbc_filename`) so script errors carry file/line
  info; `mrb_load_nstring_cxt` replaces the manual parse/generate/run sequence.
- After any `mrb_funcall` from C, check `mrb->exc` before using the return value.
