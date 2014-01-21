#include <iostream>
#include <forward_list>
#include <iterator>

using namespace std;

int main(int argc, const char *argv[])
{

  forward_list<int> l = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  l.erase_after(l.before_begin());
  for (auto n : l) cout << n << " "; cout << endl;

  auto fi = next(l.begin());
  auto la = next(fi, 3);
  
  l.erase_after(fi, la);
  for (auto n : l) cout << n << " "; cout << endl;
  
  return 0;
}
