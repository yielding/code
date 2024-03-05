#include <algorithm>
#include <fstream>
#include <iostream>

using namespace std;

template <typename Iterator>
auto min_vector(Iterator beg, Iterator end) -> int
{
  auto dist = distance(beg, end);
  if (dist < 1)
    throw runtime_error("error");

  return dist == 1 ? *beg : min(*beg, min_vector(beg + 1, end));
}

int main(int argc, char* argv[])
{
  vector<int> v = {1, 2, 3, 4, 5};
  cout << min_vector(v.begin(), v.end());

  return 0;
}
