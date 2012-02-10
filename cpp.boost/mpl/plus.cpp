#include <boost/mpl/plus.hpp>

#include <iostream>

using namespace std;
using namespace boost;

struct plus_f
{
  template <typename T1, typename T2>
  struct apply
  {
    typedef typename mpl::plus<T1, T2>::type type;
  };
};

int main(int argc, char const* argv[])
{
  return 0;
}
