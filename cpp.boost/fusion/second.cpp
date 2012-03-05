#include <boost/fusion/sequence.hpp>

#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/filter_if.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/type_traits/is_pointer.hpp>

#include <typeinfo>
#include <string>
#include <iostream>

using namespace std;
      namespace fusion = boost::fusion;

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
         << x.to_str()
         << "</" << typeid(x).name() << '>' ;
  }
};

int main(int argc, char const* argv[])
{
  A a;
  B b;
  C* c = new C;
  fusion::vector<A, B, C*> stuff(a, b, c);
  // fusion::for_each(stuff, print_xml());
  fusion::for_each(result_of::filter_if<is_pointer<_> >(stuff), print_xml());

  return 0;
}
