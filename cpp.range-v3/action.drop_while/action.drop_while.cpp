#include <iostream>
#include <vector>

#include <range/v3/all.hpp>

using namespace ranges;
using namespace std;

int main(int argc, char* argv[])
{
  auto v0 = views::ints(1, 21) | to<vector>();
  v0 |= actions::drop_while([](int i) { return i < 10; });
  
  v0 |= actions::drop_while([](int) { return true; });
  assert(v0.size() == 0);
  
  return 0;
}

