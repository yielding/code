#include <iostream>
#include <memory>

using namespace std;

struct test
{
  void f()&  { cout << "lvalue obj\n"; }
  void f()&& { cout << "rvalue obj\n"; }
};

struct test2
{
  unique_ptr<int[]> heavy_reource;

  test2() : heavy_reource(new int[0x400])
  {}

  operator unique_ptr<int[]>() const&
  {
    unique_ptr<int[]> p(new int[0x400]);
    for (int i=0; i<0x400; ++i) p[i] = heavy_reource[i];

    cout << "heavy resource\n";

    return p;
  }

  operator unique_ptr<int[]>() &&
  {
    cout << "move resource\n";

    return move(heavy_reource);
  }
};

int main(int argc, char *argv[])
{
  // test t; t.f(); test().f();
  test2 t2;
  auto cp = unique_ptr<int[]>(t2);
  auto mv = unique_ptr<int[]>(test2());


  return 0;
}
