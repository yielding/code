#include <iostream>
#include <vector>
#include <algorithm>
#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace boost;

template <typename C> 
void pr(C& c)
{
  for_each(c.begin(), c.end(), cout << lambda::_1 << " ");
  cout << endl;
}

int main(int argc, char const* argv[])
{
  vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vector<int> r;

  auto c = [](int i) { return i > 5; };

  remove_copy_if(v.begin(), v.end(), back_inserter(r), c);
  pr(r);
  
  return 0;
}
