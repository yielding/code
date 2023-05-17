#include <iostream>
#include <algorithm>
#include <tuple>
#include <format>
#include <vector>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
   auto triples = v::for_each(v::iota(1), [](int z) {
        return v::for_each(v::iota(1, z + 1), [=](int x) {
            return v::for_each(v::iota(x, z + 1), [=](int y) {
                return g::yield_if(x * x + y * y == z * z,
                                make_tuple(x, y, z));
            });
        });
    });

  for (auto const& [e1, e2, e3]: triples | v::take(5)) 
    cout << format("({}:{}:{})", e1, e2, e3) << endl;

  return 0;
}