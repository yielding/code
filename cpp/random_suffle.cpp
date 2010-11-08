#include <iostream>
#include <algorithm>
#include <numeric>
#include <vector>

#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace boost::lambda;

template<typename T>
void print(T const& col, char const* title="")
{
  cout << title;
  for_each(col.begin(), col.end(), cout << _1 << " ");
  cout << endl;
}

int main()
{
  std::vector<int> ar(20, 0);

  int i=0; for_each(ar.begin(), ar.end(), _1 = ++var(i));
  print(ar);

  random_shuffle(ar.begin(), ar.end());
  print(ar);
}
