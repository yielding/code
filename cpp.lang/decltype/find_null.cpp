#include <iostream>
#include <vector>

using namespace std;

template <typename Cont>
auto find_null(const Cont& c) -> decltype(begin(c))
{
  auto it = begin(c);
  for (; it != end(c); ++it)
    if (*it == 0)
      break;

  return it;
}

int main(int argc, const char *argv[])
{
  vector<int> v { 1, 2, 3, 4, 0, 6 };

  auto it = find_null(v);

  cout << it - v.begin();

  return 0;
}
