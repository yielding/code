#include <boost/fusion/container/map.hpp>
#include <boost/fusion/mpl.hpp>
#include <boost/mpl/fold.hpp>
#include <iostream>

using namespace boost;
using namespace std;

int main(int argc, char** argv)
{
  typedef boost::fusion::map<
    fusion::pair<int, const char*>,
    fusion::pair<long, char>
  > FuMap;

  FuMap fuMap("hello", 'w');

  unsigned val = mpl::fold<
    FuMap, mpl::int_<0>, mpl::next<mpl::_1>
  >::type::value;

  cout << val;
}
