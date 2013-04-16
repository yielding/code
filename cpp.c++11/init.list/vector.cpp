#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main(int argc, char const* argv[])
{
  vector<int> v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  auto is_even = [](int x) { return x % 2 == 0; };
  v.erase(remove_if(v.begin(), v.end(), is_even), v.end());

  for (auto x: v) cout << x << " ";
  
  return 0;
}
