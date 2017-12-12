#ifndef STATEMENT_H_JLHZK1YM
#define STATEMENT_H_JLHZK1YM

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#include "statement_handle.h"
#include "connection.h"
#include <cstring>
#include <string>

struct statement
{
  auto prepare(connection const& c, char const* text) -> bool
  {
    handle_.reset();

    auto result = ::sqlite3_prepare_v2(c.handle_.get(),
        text,
        ::strlen(text),
        handle_.get_address_of(),
        nullptr);


    return result == SQLITE_OK;
  }

  auto bind(int const index, int const value) -> bool
  {
    auto result = ::sqlite3_bind_int(handle_.get(), index, value);

    return result == SQLITE_OK;
  }

  auto bind(int const index, char const* value) -> bool
  {
    auto result = ::sqlite3_bind_text(handle_.get(), index, value, ::strlen(value), SQLITE_STATIC);

    return result == SQLITE_OK;
  }

  auto step() -> bool
  {
    auto result = ::sqlite3_step(handle_.get());

    if (result == SQLITE_ROW)  return true;
    if (result == SQLITE_DONE) return false;

    return false;
  }

  auto reset() -> bool
  {
    auto result = ::sqlite3_reset(handle_.get());

    return result == SQLITE_OK;
  }

  // class reader 
  
  auto get_int(int const col=0) -> int
  {
    return ::sqlite3_column_int(handle_.get(), col);
  }

  auto get_int64(int const col=0) -> int64_t
  {
    return (int64_t)::sqlite3_column_int64(handle_.get(), col);
  }

  auto get_string(int const col) -> std::string
  {
    auto res = (char*)::sqlite3_column_text(handle_.get(), col);

    return res;
  }


private:
  statement_handle handle_;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
