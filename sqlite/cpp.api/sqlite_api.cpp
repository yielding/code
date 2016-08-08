#include "sqlite_api.h"

#include <sqlite3.h>
#include <stdexcept>

#include <boost/format.hpp>

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SQLiteApi::impl
{
public:
  impl(char const* path): m_path(path)
  {
    m_db = nullptr;
    m_opened = false;
  }

  ~impl()
  {
    if (m_db != nullptr)
      ::sqlite3_close(m_db);
  }

  auto open() -> bool 
  {
    sqlite3* db = nullptr;
    if (::sqlite3_open_v2(m_path, &db, SQLITE_OPEN_READONLY, nullptr) != SQLITE_OK)
      return false;

    m_db = db;
    m_opened = true;

    return true;
  }

public:
  auto schema(char const* ofTable) const -> string
  {
    auto result = ""s;
    auto sql  = str(format("select sql from sqlite_master where name='%s';") % ofTable);
    auto stmt = prepare_stmt(sql.c_str());
    if (stmt == nullptr)
      return result;

    if (::sqlite3_step(stmt) == SQLITE_ROW)
    {
      result = get_str(stmt, 0);
      ::sqlite3_finalize(stmt);
    }

    return result;
  }

  auto table_list() const -> vector<string>
  {
    vector<string> result;

    auto stmt = prepare_stmt("select name from sqlite_master where type='table'");
    if (stmt == nullptr)
      return result;

    while (::sqlite3_step(stmt) == SQLITE_ROW)
      result.push_back(get_str(stmt, 0));

    ::sqlite3_finalize(stmt);

    return result;
  }

  auto table_info(char const* name) const -> map<string, string>
  {
    map<string, string> result;

    auto sql  = str(format("pragma table_info(%s)") % name);
    auto stmt = prepare_stmt(sql.c_str());
    if (stmt != nullptr)
    {
      while (::sqlite3_step(stmt) == SQLITE_ROW)
      {
        auto id    = get_int(stmt, 0); result["id"]   = to_string(id);
        auto name  = get_str(stmt, 1); result["name"] = name;
        auto type  = get_str(stmt, 2); result["type"] = type;
        auto nn    = get_str(stmt, 3); result["nn"]   = nn;
        auto defv  = get_str(stmt, 4); result["defv"] = defv;
        auto pk    = get_str(stmt, 5); result["pk"]   = pk;
      }
    }

    return result;
  }

protected:
  auto prepare_stmt(char const* sql) const -> sqlite3_stmt*
  {
    sqlite3_stmt* stmt = nullptr;

    if (::sqlite3_prepare_v2(m_db, sql, -1, &stmt, nullptr) != SQLITE_OK)
      throw runtime_error("sqlite3 prepare stmt error");

    return stmt;
  }

private:
  auto get_int(sqlite3_stmt* stmt, int index) const -> int
  {
    return ::sqlite3_column_int(stmt, index);
  }

  auto get_str(sqlite3_stmt* stmt, int index) const -> string
  {
    auto c0 = ::sqlite3_column_text(stmt, index);

    return c0 == nullptr 
      ? ""
      : string((const char*)c0);
  }

private:
  sqlite3* m_db;
  char const* m_path;
  bool m_opened;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SQLiteApi::SQLiteApi(char const* name)
  : pimpl(new impl(name))
{
}

SQLiteApi::~SQLiteApi()
{
  pimpl.reset();
}

auto SQLiteApi::open() const -> bool
{
  return pimpl->open();
}

auto SQLiteApi::schema(char const* ofTable) const -> string
{
  return pimpl->schema(ofTable);
}

auto SQLiteApi::table_list() const -> vector<string>
{
  return pimpl->table_list();
}

auto SQLiteApi::table_info(char const* name) const -> map<string, string>
{
  return pimpl->table_info(name);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
