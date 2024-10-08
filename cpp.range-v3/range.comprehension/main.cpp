#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main()
{
  using v::ints, v::for_each, v::all, v::repeat_n, g::yield_from;
  
  auto rng = ints(1, 10)            // [1, 2, 3, 4, ..., 9]
           | for_each([](int i) {   // [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, ...]
               return yield_from(repeat_n(i, i)); });

  cout << v::all(rng) << endl;

  return 0;
}