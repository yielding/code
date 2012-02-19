#include <iostream>
#include <boost/phoenix/bind/bind_member_function.hpp>
#include <boost/phoenix/bind.hpp>
#include <boost/phoenix/core.hpp>

struct A
{
  A(int b) : m_b(b) {}

  int add(int a)
  {
    return a + m_b;
  }

  int m_b;
};

using namespace std;
using namespace boost::phoenix;
using namespace boost::phoenix::arg_names;

int main()
{

  A a(10);
  auto x = bind(&A::add, a, arg1);
  cout << x(10) << endl;

  auto y = bind(&A::m_b, a)();
  cout << y;


  // cout << (arg1->*&A::add)(arg2)(&a, 10);
}
