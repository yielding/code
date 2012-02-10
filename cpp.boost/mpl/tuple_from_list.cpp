#include <boost/mpl/reverse_fold.hpp>
#include <boost/mpl/list.hpp>
#include <boost/tuple/tuple.hpp>

#include <iostream>

using namespace boost::mpl;
using namespace boost;

template <typename Types> 
struct tuple_gen
  : reverse_fold<Types, tuples::null_type, tuples::cons<_2,_1>>
{
};

int main()
{
  tuple_gen<list<int, char const*, bool>>::type t;

  get<0>(t) = -1;
  get<1>(t) = "text";
  get<2>(t) = false;

  std::cout
    << get<0>(t) << '\n'
    << get<1>(t) << '\n'
    << get<2>(t) << '\n'
    ;

  return 0;
}
