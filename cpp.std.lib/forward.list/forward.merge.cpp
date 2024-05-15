#include <iostream>
#include <forward_list>

using namespace std;

ostream& operator<<(ostream& os, const forward_list<int>& list)
{
  for (auto& i : list) os << " " << i;

  return os;
}

int main(int argc, const char *argv[])
{
  forward_list<int> l1 = { 5, 9, 0, 1, 3 };
  forward_list<int> l2 = { 8, 7, 2, 6, 4 };

  l1.sort();
  l2.sort();

  cout << "list1 : " << l1 << endl;
  cout << "list2 : " << l2 << endl;

  l1.merge(l2);

  cout << "merged: " << l1 << endl;
  
  return 0;
}
