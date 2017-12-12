#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <string>

using namespace std;

struct sqlite_exception
{
  sqlite_exception(int const result, char const* text)
    : code    { result }
    , message { text   }
  {}

  int code;
  string message;
}

#endif
