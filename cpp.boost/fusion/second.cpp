#include <boost/mpl/placeholders.hpp>
#include <boost/fusion/sequence.hpp>

#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/filter_if.hpp>
#include <boost/fusion/include/make_vector.hpp>

#include <boost/fusion/container/vector.hpp>

#include <boost/type_traits/is_pointer.hpp>
#include <boost/type_traits/is_integral.hpp>

#include <typeinfo>
#include <string>
#include <iostream>
#include <cassert>

using namespace std;
      namespace fusion = boost::fusion;
      namespace mpl    = boost::mpl;

struct A
{
  A() {}
  string to_str() { return "I'm A\n"; }
};

struct B
{
  B() {}
  string to_str() { return "I'm B\n"; }
};

struct C
{
  C() {}
  string to_str() { return "I'm C\n"; }
};

struct print_xml
{
  template <typename T> 
  void operator() (T& x) const
  {
    cout << '<' << typeid(x).name() << '>'
         //<< x.to_str()
         << x->to_str()
         << "</" << typeid(x).name() << '>' ;
  }
};

int main(int argc, char const* argv[])
{
  A a;
  B b;
  C* c = new C;
  fusion::vector<A, B, C*> const stuff(a, b, c);

  // 1. usage 
  // fusion::vector<int, int> const vec(1, 2);

  // usage 2
  fusion::for_each(
    fusion::filter_if<boost::is_pointer<mpl::_> >(stuff), print_xml());

  return 0;
}
