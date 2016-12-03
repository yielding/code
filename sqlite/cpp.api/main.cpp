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

    cout << db.schema("images") << endl;
    for (auto name: db.table_list()) cout << name << endl;
    
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
