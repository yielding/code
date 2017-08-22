#ifndef IO_SQLITE_HPP_
#define IO_SQLITE_HPP_

#include "sqlite3.h"

#include <exception>
#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>
#include <iostream>
#include <map>

namespace io::sqlite {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;
  
class error: public std::exception
{
public:
  explicit error(int c) : _code(c) { }
  virtual ~error() throw() { }

  const char *what() const throw() override
  {
    return "SQLite Error";
  }

  int code() const
  {
    return _code;
  }

private:
  int _code;
};

// Check return code, throw error if not ok
namespace impl {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
static inline void check(int c)
{
  if (c != SQLITE_OK)
    throw error(c);
}

inline void destroy_blob(void *blob)
{
  delete[] reinterpret_cast<uint8_t *>(blob);
}

inline void destroy_text(void *blob)
{
  delete[] reinterpret_cast<char *>(blob);
}

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class db 
{
public:
  db() : _db(nullptr) { }

  explicit db(const std::string& filename)
  {
    impl::check(sqlite3_open(filename.c_str(), &_db));
  }

  explicit db(char const* filename)
  {
    impl::check(sqlite3_open(filename, &_db));
  }

  ~db()
  {
    if (_db)
      sqlite3_close(_db);
  }

  db(const db&) = delete;
  db& operator=(const db&) = delete;

  void swap(db& r)
  {
    std::swap(_db, r._db);
  }

  db(db&& r) : _db(r._db)
  {
    r._db = nullptr;
  }

  db& operator=(db&& r)
  {
    db m(std::move(r));

    swap(m);
    return *this;
  }

  auto get() -> ::sqlite3*
  {
    return _db;
  }

  auto get() const -> ::sqlite3 const*
  {
    return _db;
  }

  // Number of changes due to the most recent statement.
  auto changes() const -> unsigned
  {
    return sqlite3_changes(_db);
  }

  // Execute a simple statement
  void exec(const std::string& text) const
  {
    impl::check(sqlite3_exec(_db, text.c_str(), nullptr, nullptr, nullptr));
  }

  void exec(const char *text) const
  {
    impl::check(sqlite3_exec(_db, text, nullptr, nullptr, nullptr));
  }
  
private:
  ::sqlite3* _db;
};

////////////////////////////////////////////////////////////////////////////////
//
// class stmt
//
////////////////////////////////////////////////////////////////////////////////
class stmt 
{
public:
  explicit stmt() : _stmt(nullptr) { }

  stmt(db& db, const char *sql)
  {
    impl::check(sqlite3_prepare_v2(db.get(), sql, -1, &_stmt, nullptr));
  }
  
  stmt(db& db, const std::string& sql)
  {
    impl::check(sqlite3_prepare_v2(db.get(), sql.c_str(), -1, &_stmt, nullptr));
  }

 ~stmt()
  {
    if (_stmt) sqlite3_finalize(_stmt);
  }

  stmt(const stmt&) = delete;
  stmt& operator=(const stmt&) = delete;

  void swap(stmt& r)
  {
    std::swap(_stmt, r._stmt);
  }

  stmt(stmt&& r) : _stmt(r._stmt)
  {
    r._stmt = nullptr;
  }

  stmt& operator=(stmt&& r)
  {
    auto m(std::move(r));

    swap(m);
    return *this;
  }

  auto get() -> ::sqlite3_stmt*
  {
    return _stmt;
  }

  auto get() const -> ::sqlite3_stmt const*
  {
    return _stmt;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // class stmt::binder
  //
  ////////////////////////////////////////////////////////////////////////////////
  class binder 
  {
  public:
    explicit binder(stmt& s) : _stmt(s._stmt) { }
    explicit binder(::sqlite3_stmt* stmt) : _stmt(stmt) { }

    auto blob(unsigned i, const void *data, size_t len) -> binder& 
    {
      auto copy = new uint8_t[len];

      memcpy(copy, data, len);
      impl::check(sqlite3_bind_blob(_stmt, i, copy, (int)len, impl::destroy_blob));

      return *this;
    }

    auto blob_ref(unsigned i, const void *data, size_t len) -> binder& 
    {
      impl::check(sqlite3_bind_blob(_stmt, i, data, (int)len, nullptr));

      return *this;
    }

    auto real(unsigned i, double value) -> binder& 
    {
      impl::check(sqlite3_bind_double(_stmt, i, value));

      return *this;
    }

    auto int32(unsigned i, int32_t value) -> binder& 
    {
      impl::check(sqlite3_bind_int(_stmt, i, value));

      return *this;
    }

    auto int64(unsigned i, int64_t value) -> binder& 
    {
      impl::check(sqlite3_bind_int64(_stmt, i, value));

      return *this;
    }

    auto null(unsigned i) -> binder& 
    {
      impl::check(sqlite3_bind_null(_stmt, i));

      return *this;
    }

    auto text(unsigned i, const char *orig) -> binder& 
    {
      const auto len = ::strlen(orig);
      auto copy = new char[len];

      ::memcpy(copy, orig, len);
      impl::check(sqlite3_bind_text(_stmt, i, copy, (int)len, impl::destroy_text));

      return *this;
    }

    auto text(unsigned i, const std::string& value) -> binder& 
    {
      auto orig = value.c_str();
      auto len  = value.size();
      auto copy = new char[len];

      ::memcpy(copy, orig, len);
      impl::check(sqlite3_bind_text(_stmt, i, copy, (int)len, impl::destroy_text));

      return *this;
    }

    auto text_ref(unsigned i, const std::string& value) -> binder& 
    {
      impl::check(sqlite3_bind_text(_stmt, i, value.c_str(), (int)value.size(), nullptr));

      return *this;
    }

    auto text_ref(unsigned i, const char *value) -> binder& 
    {
      impl::check(sqlite3_bind_text(_stmt, i, value, -1, nullptr));

      return *this;
    }

    void clear() const
    {
      impl::check(sqlite3_clear_bindings(_stmt));
    }

  private:
    ::sqlite3_stmt *_stmt;
  };

  auto bind() -> binder
  {
    return binder(*this);
  }
  
  auto step() const -> bool
  {
    const auto c = sqlite3_step(_stmt);
  
    if (c == SQLITE_ROW)  return true;
    if (c == SQLITE_DONE) return false;
  
    throw error(c);
  }
  
  auto exec() const -> void
  {
    while (step());
  }

  auto reset() const -> void
  {
    impl::check(sqlite3_reset(_stmt));
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // class reader
  //
  ////////////////////////////////////////////////////////////////////////////////
  class reader 
  {
  public:
    explicit reader(stmt& s) : _stmt(s._stmt) { }

    auto blob(unsigned i) const -> void const*
    {
      return sqlite3_column_blob(_stmt, i);
    }

    auto size(unsigned i) const -> size_t
    {
      return sqlite3_column_bytes(_stmt, i);
    }

    auto real(unsigned i) const -> double
    {
      return sqlite3_column_double(_stmt, i);
    }

    auto int32(unsigned i) const -> int32_t 
    {
      return sqlite3_column_int(_stmt, i);
    }
    
    auto int64(unsigned i) const -> int64_t
    {
      return sqlite3_column_int64(_stmt, i);
    }

    auto cstr(unsigned i) const -> char const*
    {
      return reinterpret_cast<const char *> (sqlite3_column_text(_stmt, i));
    }

    auto text(unsigned i) const -> std::string 
    {
      return std::string(cstr(i), size(i));
    }

  private:
    ::sqlite3_stmt *_stmt;
  };

  stmt::reader row()
  {
    return stmt::reader(*this);
  }

private:
  ::sqlite3_stmt *_stmt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class schema
{
public:
  static auto column_defs(string const& tname, string const& sql) -> vector<map<string, string>>
  {
    vector<map<string, string>> result;
    
    try
    {
      db _db(":memory:");
      _db.exec(sql);
      
      auto sql = string("pragma table_info(") + tname + ")";
      stmt _stmt(_db, sql);
      
      for (int index=0; _stmt.step(); index++)
      {
        map<string, string> item;
        auto row  = _stmt.row();
        auto id   = row.int32(0); item["id"]   = to_string(id);
        auto name = row.text(1);  item["name"] = name;
        auto type = row.text(2);  item["type"] = type;
        auto nn   = row.text(3);  item["nn"]   = nn;
        auto defv = row.text(4);  item["defv"] = defv;
        auto ispk = row.text(5);  item["pk"]   = ispk;
        
        result.push_back(item);
      }
      
      _db.exec(string("drop table ") + tname);
    }
    catch (error& e)
    {
      cout << e.what() << endl;
    }
      
    return result;
  }
};
      
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

}

#endif
