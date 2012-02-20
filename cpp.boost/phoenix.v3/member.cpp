#include <iostream>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator/member.hpp>

using namespace std;

struct A
{
  A(int m):member(m) {}

  int add(int addend)
  {
    return member + addend;
  }

  int member;
};

int main(int argc, char const* argv[])
{
  using namespace boost::phoenix::arg_names;

  A* a = new A(10);

  cout << (arg1->*&A::member)(a) << endl;
  cout << (arg1->*&A::add)(arg2)(a, 10) << endl;

  delete a;
 
  return 0;
}
