#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  using g::to, v::ints, a::take_while;
  
  auto v = ints(1, 21) | to<vector>();
  auto& v2 = take_while(v, [](int i) { return i < 18; });

  assert(v2.size() == 17);

  v |= take_while([](int i) { return i < 15; });
  assert(v.size() == 14);
  
  v |= take_while([](int) { return true; });
  assert(v.size() == 14u);

  v |= take_while([](int) { return false; });
  assert(v.size() == 0u);

  return 0;
}
