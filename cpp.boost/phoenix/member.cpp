#include <vector>
#include <iostream>
#include <algorithm>
#include <boost/phoenix/bind/bind_member_function.hpp>
#include <boost/phoenix/bind.hpp>
#include <boost/phoenix/core.hpp>

#include <boost/phoenix/operator/member.hpp>

using namespace std;
using namespace boost::phoenix;
using namespace boost::phoenix::arg_names;

struct A
{
  A(int b) : m_b(b) {}

  int add(int a)
  {
    return a + m_b;
  }

  int& m_b;
};

int main()
{
  vector<A> v;

  v.push_back(A(10));
  v.push_back(A(20));

  auto const it = find_if(v.begin(), v.end(), ((arg1)->*&A::m_b) == 5);

  /*
  A a(10);
  auto x = bind(&A::add, a, arg1);
  cout << x(10) << endl;

  auto y = bind(&A::m_b, a)();
  cout << y << endl;

  auto z = (arg1->*&A::m_b)(&a);
  cout << z << endl;

  auto k = (arg1->*&A::add)(arg2)(&a, 2);
  cout << k << endl;
  */
}
