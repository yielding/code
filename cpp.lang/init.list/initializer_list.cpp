#include <iostream>
#include <iterator>
#include <vector>
#include <initializer_list>

using namespace std;

template <typename T> 
struct S
{
  S(initializer_list<T> l): v(l) 
  {
    cout << "constructed with a " << l.size() << "-element list\n";
  }

  vector<T> v;
};

int main(int argc, char const* argv[])
{
  // S<int> s = { 1, 2, 3, 4, 5 };
  S s = { 1, 2, 3, 4, 5 };

  for (auto n: s.v) 
    cout << n << " ";

  cout << endl;

  auto it = next(s.v.begin(), 3);
  cout << *it;

  return 0;
}
