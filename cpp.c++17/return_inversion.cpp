#include <iostream>
#include <string>

using namespace std;

template <typename F> 
void f(F f)
{
  f(10);
}

int main(int argc, char const* argv[])
{
  int x = 10;
  int y = 20;

  string s;

  auto r = [&](int a) { x += a; y += a + 10; };

  f(r);
  cout << x << " " << y << endl;
  
  return 0;
}
