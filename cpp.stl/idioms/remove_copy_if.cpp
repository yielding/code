#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main(int argc, char const* argv[])
{
  vector<int> v{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vector<int> c;

  remove_copy(v.begin(), v.end(), 
      back_inserter(c),
      5);

  auto f = [](int i) { cout << i << " "; };

  for_each(v.begin(), v.end(), f); cout << endl;
  for_each(c.begin(), c.end(), f); cout << endl;

  return 0;
}
