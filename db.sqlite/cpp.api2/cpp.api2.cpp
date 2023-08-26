#include "sqlite_pp.hpp"

#include <iostream>
#include <vector>
#include <string>
#include <format>

using namespace io::sqlite;
using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto table_list(db& conn) -> vector<string>
{
  stmt s(conn, "select name from sqlite_master where type='table'");

  vector<string> result;
  while (s.step()) 
  {
    auto row = s.row();
    result.push_back(row.text(0));
  }

  return result;
}

auto schema(db& conn, char const* of_table) -> string
{
  auto sql = format("select sql from sqlite_master where name='{}';", of_table); 

  stmt s(conn, sql.c_str());

  return (s.step())
    ? s.row().text(0)
    : ""s;
}

auto table_info(db& conn) -> vector<string>
{
  stmt s(conn,"pragma table_info(images)");

  while (s.step())
  {
    auto row  = s.row();
    auto id   = row.int32(0);
    auto name = row.text(1);

    cout << format("id: {}, name: [{}]\n", id, name);
  }

  return vector<string>();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  db maptile(":memory:");
  maptile.exec("CREATE TABLE images(zoom int, x int, y int, flags int, length int, data blob)");
  auto sql = schema(maptile, "images");
  cout << sql << endl;
  // table_info(maptile);

  maptile.exec("drop TABLE images");
  auto tables = table_list(maptile);
  for (auto table: tables) 
    cout << table << endl;

  /*
  db maptile("/Users/yielding/work/swift/data/maptile.db");
  auto sql = schema(maptile, "images");
  cout << sql << endl;

  auto tables = table_list(maptile);
  for (auto table: tables) cout << table << endl;
  */

  return 0;
}
