#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

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

  return 0;
}
