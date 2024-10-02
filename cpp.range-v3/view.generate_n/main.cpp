#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main() 
{
  auto rng = v::generate_n([x=1]() mutable { return (x <<= 1) >> 1; }, 10);
  cout << v::all(rng) << endl;

  return 0;
}
