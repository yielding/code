#include "sqlite3/conn.hpp"
#include "sqlite3/stmt.hpp"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  using namespace api::sqlite3;

  conn c;
  c.open("aaa.db");
  c.execute("create table hens(id int primary key, name text)");

  stmt s;
  s.prepare(c, "insert into hens(id, name) values(?, ?)");

  s.bind(1, 101);
  s.bind(2, "henrietta");
  s.step();
  s.reset();

  s.bind(1, 102);
  s.bind(2, "duck");
  s.step();

  return 0;
}
