#pragma once

#include <expected>
#include <string>
#include <system_error>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace domain
{
  using namespace std;

  enum class Source { File, Net, Db, Unknown };

  struct DomainError 
  {
    error_code code;
    Source   src = Source::Unknown;
    string   what;
    string   where;
    string   op;
    string   trace;

    auto message() const -> string 
    {
      string s = code.message();
      auto src_name = [](const Source s) {
        switch (s) 
        {
          case Source::File: return "file";
          case Source::Net:  return "net";
          case Source::Db:   return "db";
          default:           return "unknown";
        }
      }(src);

      s += " src="; s += src_name;
      if (!op.empty())    { s += " op=";    s += op; }
      if (!where.empty()) { s += " where="; s += where; }
      if (!what.empty())  { s += " what=";  s += what; }
      if (!trace.empty()) { s += " trace="; s += trace; }

      return s;
    }
  };

  template<class T>
  using Ex = expected<T, DomainError>;

  auto Err(const error_code ec, const Source src,
    string what = {},
    string where = {},
    string op = {},
    string trace = {}) -> unexpected<DomainError> 
  {
    return unexpected<DomainError>{ 
      {
       .code  = ec, 
       .src   = src, 
       .what  = std::move(what),
       .where = std::move(where), 
       .op    = std::move(op), 
       .trace = std::move(trace)
      }
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
