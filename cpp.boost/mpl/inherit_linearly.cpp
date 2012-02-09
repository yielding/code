#include <boost/mpl/vector.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <iostream>
#include <typeinfo>

using namespace boost::mpl::placeholders;

typedef boost::mpl::vector<short[2], long, char*, int> member_types;

template <typename T>
struct wrap
{
  T value;
};

struct print
{
  template <typename T>
  void operator()(T)
  {
    std::cout << typeid(T).name() << std::endl;
  }
};

typedef boost::mpl::inherit_linearly<member_types, boost::mpl::inherit<wrap<_2>, _1> >::type Generate;

int main()
{
  Generate generated;
  print p;

  std::cout <<static_cast<wrap<int>&>(generated).value << std::endl;

  boost::mpl::for_each<member_types>(p);

  return 0;
}

