#include <iostream>

#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/vector/vector30.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/inserter.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/back.hpp>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;
using namespace std;

typedef mpl::vector <
  mpl::vector_c<int, 0, 1, 2>,
  mpl::vector_c<int, 3, 4, 5>,
  mpl::vector_c<int, 5, 6, 7>
> S;

typedef mpl::transform<
  S,
  mpl::back<_>,
  mpl::inserter <
    mpl::int_<1>,
    mpl::plus<_, _>
  >
>::type sum;

template <typename T>
struct tuple_field
{
  typedef tuple_field type;
  T m_field;
};

template <typename T>
inline T& field(tuple_field<T>& t)
{
  return t.m_field;
}

typedef mpl::inherit_linearly <
  mpl::list<int, const char*, bool>, 
  mpl::inherit<_1, tuple_field<_2> >
>::type tuple_t;

int main(int argc, char* argv[]) 
{
  tuple_t t;

  field<int>(t) = -1;
  field<const char*>(t) = "text";
  field<bool>(t) = false;

  cout << field<int>(t) << endl
       << field<const char*>(t) << endl
       << field<bool>(t) << endl;

  cout << sum::value;

  return 0;
}
