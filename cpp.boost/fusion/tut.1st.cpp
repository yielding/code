#include <boost/fusion/sequence.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/include/sequence.hpp>

#include <string>
#include <iostream>

using namespace std;
      namespace fusion = boost::fusion;

int main(int argc, char const* argv[])
{
  fusion::vector<int, char, string> stuff(1, 'x', "howdy");

  auto i = fusion::at_c<0>(stuff);
  auto c = fusion::at_c<1>(stuff);
  auto s = fusion::at_c<2>(stuff);

  cout << s;

  return 0;
}
