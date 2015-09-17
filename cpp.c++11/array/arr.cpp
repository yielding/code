#include <iostream>
#include <string>
#include <algorithm>
#include <array>
#include <iterator>

using namespace std;

int main(int argc, const char *argv[])
{
  auto a1 = array<int, 3> { {1, 2, 3} };
  auto a2 = array<int, 3> { 1, 2, 3 };
  array<string, 2> a3 = { string("a"), "b" };

  sort(a1.begin(), a1.end());
  reverse_copy(a2.begin(), a2.end(), ostream_iterator<int>(cout, " "));
  cout << "\n";

  for (auto& s: a3)
    cout << s << " ";
  
  return 0;
}
