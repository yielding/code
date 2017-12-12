#include "cpp.api3.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  connection c;
  c.open("aaa.db");
  c.execute("create table hens(id int primary key, name text)");

  statement s;
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
