#include "sqlite_api.h"
#include <iostream>
#include <string>
#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  try 
  {
    SQLiteApi db("/Users/yielding/work/swift/data/maptile.db");
    if (!db.open())
    {
      cout << "error\n";
      return 1;
    }

    // 0. list schema
    for (auto name: db.table_list()) cout << name << endl;

    // 1. read schema of given table
    cout << db.schema("images") << endl;

    // 2. show columns of table
    auto defs = db.table_info("images");
    for (auto d: defs) cout << d << endl;
  }
  catch(exception e)
  {
    cout << e.what();
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
