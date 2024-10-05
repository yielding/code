#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  using v::all, v::enumerate, v::for_each, g::yield_if;

  auto data = { 7, 0, 2, 3, 1, 4, 6 };
  for (auto [i, v] : data | enumerate)
    cout << "index: " << i << " , " << "value: " << v << endl;

  auto rng = data 
    | enumerate 
    | for_each([](auto const& p) { return yield_if(p.first == p.second, p.second); });

  cout << all(rng) << endl;

  return 0;
}