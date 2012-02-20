#include <iostream>
#include <vector>
#include <algorithm>
#include <boost/shared_ptr.hpp>

#include <boost/phoenix/core.hpp>
#include <boost/phoenix/object/new.hpp>
#include <boost/phoenix/object/delete.hpp>
#include <boost/phoenix/object/construct.hpp>
#include <boost/phoenix/operator.hpp>

#include <boost/detail/lightweight_test.hpp>

using namespace boost::phoenix;
using namespace boost::phoenix::arg_names;
using namespace std;

int n = 0;

struct X
{
  X(int, int, int) { cout << "new X(int, int, int)" << endl; ++n; }
  X() { cout << "new X" << endl; ++n; }
 ~X() { cout << "delete X" << endl; --n; }
};

int main()
{
  vector<X*> v(10);

  // lazy new_ for_each의 loop 안에서 호출된다.
  for_each(v.begin(), v.end(), arg1 = new_<X>());
  for_each(v.begin(), v.end(), delete_(arg1));

  for_each(v.begin(), v.end(), arg1 = new_<X>(1, 2, 3));
  for_each(v.begin(), v.end(), delete_(arg1));

  typedef boost::shared_ptr<X> Xsp;

  vector<Xsp> v1(10);
  for_each(v1.begin(), v1.end(),
      arg1 = construct<Xsp>(new_<X>()));

  BOOST_TEST(n == 0);
  return boost::report_errors();
}
