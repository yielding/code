#pragma once

#include <system_error>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace db
{
  using namespace std;

  enum class DbErr { Ok = 0, ConnFail, Deadlock, Constraint, Syntax };

  struct db_error_category : error_category 
  {
    auto name() const noexcept -> const char* override 
    { 
      return "db"; 
    }

    auto message(const int ev) const -> string override 
    {
      switch (static_cast<DbErr>(ev)) 
      {
        case DbErr::Ok:    return "ok";
        case DbErr::ConnFail:  return "connection failure";
        case DbErr::Deadlock:  return "deadlock";
        case DbErr::Constraint:return "constraint violation";
        case DbErr::Syntax:  return "syntax error";
      }
      return "unknown db error";
    }
  };

  auto db_category() -> const db_error_category& 
  { 
    static db_error_category c; return c; 
  }

  auto make_error_code(const DbErr e) -> error_code 
  { 
    return { static_cast<int>(e), db_category() }; 
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
