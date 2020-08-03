#pragma once

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#include "connection_handle.h"

struct connection
{
  connection_handle handle_;

  auto open(char const* filename = ":memory:") -> bool
  {
    auto local = connection_handle {};
    auto result = ::sqlite3_open(filename, local.get_address_of());
    if (result != SQLITE_OK)
      return false;

    handle_ = std::move(local);

    return true;
  }

  auto create_memory() -> bool
  {
    auto local = connection_handle{};
    auto result = ::sqlite3_open(":memory:", local.get_address_of());

    handle_ = std::move(local);

    return result == SQLITE_OK;
  }

  auto execute(char const* text) -> bool 
  {
    auto result = ::sqlite3_exec(handle_.get(),
                      text,
                      nullptr,
                      nullptr,
                      nullptr);

    return result == SQLITE_OK;
  }

  auto rowid() -> int64_t
  {
    return (int64_t)::sqlite3_last_insert_rowid(handle_.get());
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
