#include "sqlite3/conn.hpp"
#include "sqlite3/stmt.hpp"

#include "scope_guard.hpp"
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  using namespace api::sqlite3;
  using namespace std;
  using namespace sg;

  conn c;
  c.open();
  c.execute("create table hens(id int primary key, name text)");

  auto g = make_scope_guard([&] { c.execute("COMMIT;"); });

  try
  {
    // Transaction support
    c.execute("BEGIN;");
    stmt s;
    s.prepare(c, "insert into hens(id, name) values(?, ?)");

    s.bind(1, 101);
    s.bind(2, "henrietta");
    s.step();
    s.reset();

    s.bind(1, 102);
    s.bind(2, "duck");
    s.step();
  }
  catch (...)
  {
    g.dismiss();
    c.execute("ROLLBACK;");
  }

  return 0;
}
