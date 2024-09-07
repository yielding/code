#include <iostream>
#include <array>
#include <vector>
#include <algorithm>

using namespace std;

int main(int argc, char* argv[])
{
  vector v{1, 2, 3, 4, 5, 6};

  cout << boolalpha
       << "vector: " << ranges::is_sorted(v) << endl;

  array a{6, 2, 3, 4, 5, 6};
  cout << "array: " << ranges::is_sorted(a) << endl;

  return 0;
}