#include <boost/iterator/reverse_iterator.hpp>
#include <boost/cstdlib.hpp>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;

int main()
{
  int x[] = { 1, 2, 3, 4 };

  boost::reverse_iterator<int*> first(x + 4), last(x);
  copy(first, last, ostream_iterator<int>(cout, " "));
  cout << endl;
  return 0;
}
