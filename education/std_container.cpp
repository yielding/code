#include <iostream>
#include <list>
#include <vector>

using namespace std;

template<typename Container>
void print1(Container& c)
{
  for (auto v: c) cout << v << " ";

  cout << endl;
}


template<typename Container>
void print2(Container& c)
{
  for (auto it=c.begin(); it != c.end(); ++it)
    cout << *it << " ";

  cout << endl;
}

int main(int argc, const char *argv[])
{
  vector<int> v = {1, 2, 3, 4, 5 };
  list<int>   l = {1, 2, 3, 4, 5 };

  print1(v);
  print1(l);
  print2(v);
  print2(l);

  return 0;
}
