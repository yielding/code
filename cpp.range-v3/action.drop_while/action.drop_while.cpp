#include <iostream>
#include <vector>

#include <range/v3/all.hpp>

namespace a = ranges::actions;
namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  using v::ints, a::drop_while, g::to;

  auto v = ints(1, 21) | to<vector>;

  v |= drop_while([](int i) { return i < 10; });
  v |= drop_while([](int i) { return true;   });

  assert(v.size() == 0);
  
  return 0;
}
