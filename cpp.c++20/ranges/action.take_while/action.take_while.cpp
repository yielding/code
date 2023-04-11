#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

using namespace ranges;
using namespace std;

int main(int argc, char* argv[])
{
  auto v = views::ints(1, 21) | to<vector>();
  auto& v2 = actions::take_while(v, [](int i) { return i < 18; });

  assert(v2.size() == 17);

  v |= actions::take_while([](int i) { return i < 15; });
  assert(v.size() == 14);
  
  v |= actions::take_while([](int) { return true; });
  assert(v.size() == 14u);

  v |= actions::take_while([](int) { return false; });
  assert(v.size() == 0u);

  return 0;
}
