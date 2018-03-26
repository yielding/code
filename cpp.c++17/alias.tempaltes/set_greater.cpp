#include <iostream>
#include <set>

using namespace std;

template <typename T> 
using set_gt = set<T, greater<T>>;

int main(int argc, const char *argv[])
{
  set_gt<double> s { 1.1, 8.7, -5.1 };

  for (auto v: s) cout << v << " ";
  cout << endl;
  
  return 0;
}
