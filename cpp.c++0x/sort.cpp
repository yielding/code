#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

void pr(vector<int> const& v)
{
  for (auto i=v.begin(); i != v.end(); ++i)
    cout << *i << " ";

  cout << endl;
}

int main(int argc, char const* argv[])
{
  vector<int> v = { 3, 1, 2, 5, 4 };

  pr(v);

  auto x1 = [] (int s1, int s2) { return s1 < s2; };
  std::sort(v.begin(), v.end(), x1);

  pr(v);

  return 0;
}
