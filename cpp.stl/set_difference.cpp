#include <algorithm>
#include <iostream>
#include <set>
#include <string>
#include <vector>

using namespace std;

void pr(vector<int> v)
{
  auto o = [](int i) { cout << i << " "; };
  for_each(v.begin(), v.end(), o); cout << endl;
}

void simple()
{
  vector<int> arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  set<int>    key = { 1, 3, 5 };
  vector<int> res;

  set_difference(arr.begin(), arr.end(), key.begin(), key.end(),
                 back_inserter(res));
  pr(res);
}

int main(int argc, char const* argv[])
{
  simple();

  return 0;
}
