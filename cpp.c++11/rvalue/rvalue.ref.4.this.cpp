#include <iostream>
#include <memory>
#include <string>

using namespace std;

struct test {
  void f() &  { cout << "lvalue object\n"; }
  void f() && { cout << "rvalue object\n"; }
};

/*
int main()
{
  test t;
  t.f();             // lvalue
  test().f();        // rvalue
}

$ clang++ -std=c++0x -stdlib=libc++ -Wall -pedantic t.cpp
$ ./a.out
*/

struct test2 
{
  test2() : heavy_resource(new int[500]) 
  {}

  operator unique_ptr<int[]> const& 
  {
    unique_ptr<int[]> p(new int[500]);
    for (int i=0; i<500; i++)
      p[i] = heavy_resource[i]
  }

  operator unique_ptr<int[]> && 
  {
    return move(heavy_resource);
  }

private:
  unique_ptr<int[]> heavy_resource;
};

int main(int argc, char *argv[])
{
  value v;

  cout << v.get_value().s;
  
  return 0;
}
