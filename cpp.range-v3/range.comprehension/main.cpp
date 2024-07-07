#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;
using namespace std;

int main()
{
  auto rng = v::ints(1, 10)
    | v::for_each([](int i) { return g::yield_from(v::repeat_n(i, i)); });

  cout << v::all(rng) << endl;

  return 0;
}