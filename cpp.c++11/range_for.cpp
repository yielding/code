#include <iostream>
#include <algorithm>
#include <vector>
#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace boost::lambda;

int main(int argc, char const* argv[])
{
  int a[] = { 1, 2, 3, 4, 5, 6, 7,8, 9, 10 };
  vector<int> v(a, a + 10);

  for_each(v.begin(), v.end(), [](int& v) {
    cout << v;
    v++;
  });

  cout << "\n";
  for (auto i : v) cout << i << " ";

  cout << "\n";
  for_each(v.begin(), v.end(), cout << _1 << " ");

  return 0;
}
