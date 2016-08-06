#include <iostream>
#include <string>
#include <vector>

#include <sqlite3.h>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SQLiteApi
{
public:
  static SQLiteApi open(char const* path)
  {
    sqlite3* db = nullptr;

    if (sqlite3_open(path, &db) != SQLITE_OK)
      throw runtime_error("sqlite3 open db error");

    return SQLiteApi(db);
  }

public:
  SQLiteApi(sqlite3* db)
  {
    m_db = db;
  }

  ~SQLiteApi()
  {
    if (m_db != nullptr)
      sqlite3_close(m_db);
  }

  auto prepare_stmt(char const* sql) -> sqlite3_stmt*
  {
    sqlite3_stmt* stmt = nullptr;

    if (sqlite3_prepare_v2(m_db, sql, -1, &stmt, nullptr) == SQLITE_OK)
      throw runtime_error("sqlite3 prepare stmt error");

    return stmt;
  };

private:
  sqlite3* m_db;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  auto db = SQLiteApi::open("/Users/yielding/work/swift/data/maptile.db");


  return 0;
}
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
