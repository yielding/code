#include <iostream>
#include <forward_list>

using namespace std;

int main()
{
  forward_list<int> l1 = { 1, 2, 3, 4, 5 };
  forward_list<int> l2 = { 10, 11, 12 };

  l2.splice_after(l2.cbegin(), l1, l1.cbegin(), l1.cend());

  for (int n : l1) cout << n << ' ';
  cout << endl;

  for (int n : l2) cout << n << ' ';
  cout << endl;
}
