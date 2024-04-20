#include <iostream>
#include <array>
#include <vector>

#include <range/v3/all.hpp>

namespace rng = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  vector v{1, 2, 3, 4, 5, 6};
  cout << boolalpha
       << "vector: " << rng::is_sorted(v) << endl;

  array a{6, 2, 3, 4, 5, 6};
  cout << "array: " << rng::is_sorted(a) << endl;

  return 0;
}