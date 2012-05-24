#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <boost/mpl/list.hpp>

#include <string>
#include <iostream>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;
using namespace std;

template<typename T>
struct tuple_field
{
  typedef tuple_field type; // note the typedef
  T field_;
};

template<typename T>
T& 
field(tuple_field<T>& t)
{
  return t.field_;
}

struct record
{
  record() {}

  record(int n, string na): no(n), name(na) {}

  int no;
  string name;
};

typedef mpl::inherit_linearly<
    mpl::list<int, char const*, bool, record>
  , mpl::inherit< _1, tuple_field<_2> >
>::type my_tuple;
    
int main()
{
  my_tuple t;

  field<int>(t) = -1;
  field<char const*>(t) = "text";
  field<bool>(t) = false;
  record r(1, "leech");
  field<record>(t) = r;

  std::cout
    << field<int>(t) << '\n'
    << field<char const*>(t) << '\n'
    << field<bool>(t) << '\n'
    << field<record>(t).name << '\n'
    ;

  return 0;
}
