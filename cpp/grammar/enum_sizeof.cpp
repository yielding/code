#include <iostream>

using namespace std;

struct A
{
  enum { size_of = 10 };
};

struct B
{
  enum { size_of = 10 + A::size_of };
};

int main(int argc, char const* argv[])
{
  cout << A::size_of << endl;
  cout << B::size_of << endl;
  
  return 0;
}
