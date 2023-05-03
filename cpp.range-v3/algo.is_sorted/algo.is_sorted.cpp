#include <iostream>
#include <array>
#include <vector>

#include <range/v3/all.hpp>

namespace rng = ranges;

int main(int argc, char* argv[])
{
  using namespace std;

  vector<int> v{1, 2, 3, 4, 5, 6};
  cout << boolalpha 
       << "vector: " << rng::is_sorted(v) << endl;

  array<int, 6> a{6, 2, 3, 4, 5, 6};
  cout << "array: " << rng::is_sorted(a) << endl;

  return 0;
}