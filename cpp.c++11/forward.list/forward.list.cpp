#include <iostream>
#include <forward_list>

using namespace std;

int main()
{
  forward_list<int> l = { 1, 100, 1, 2, 3, 10, 1, 11, -1, 12 };
  l.remove(1);
  l.remove_if([](int n) { return n> 10; });

  for (auto n: l) cout << n << ' ';

  cout << '\n';
}
