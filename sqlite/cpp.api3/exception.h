#pragma once

#include <string>

struct sqlite_exception
{
  sqlite_exception(int const result, char const* text)
    : code    { result }
    , message { text   }
  {}

  int code;
  std::string message;
}
