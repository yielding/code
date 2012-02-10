#include <boost/mpl/int.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <iostream>
#include <typeinfo>

#include <iostream>

using namespace mpl::placeholders;
      namespace mpl = boost::mpl;

template<typename Base, typename T>
struct tuple_part : Base
{
  typedef tuple_part type; // note the typedef
  typedef typename Base::index::next index;

  friend T& field(tuple_part& t, index) { return t.field_; }
  T field_;
};

struct empty_tuple
{
  typedef mpl::int_<-1> index;
};

typedef mpl::inherit_linearly <
    mpl::list<int, char const*, bool>
  , tuple_part<_,_>
  , empty_tuple
>::type my_tuple;
    
int main_1()
{
    my_tuple t;
    
    field(t, mpl::int_<0>()) = -1;
    field(t, mpl::int_<1>()) = "text";
    field(t, mpl::int_<2>()) = false;

    std::cout
        << field(t, mpl::int_<0>()) << '\n'
        << field(t, mpl::int_<1>()) << '\n'
        << field(t, mpl::int_<2>()) << '\n'
        ;

    return 0;
}

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
