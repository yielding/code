#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

namespace a = ranges::actions;
using namespace std;

int main(int argc, char* argv[])
{
  using a::unstable_remove_if;

  auto make_vector = []() { return vector{1, 2, 3, 4, 5}; };

  vector<int> v0;
  v0 |= unstable_remove_if([](int) { return true; });
  assert(v0.empty());

  auto v1 = make_vector();
  v1 |= unstable_remove_if([](int) { return false; });
  assert(v1.size() == 5);

  auto v2 = make_vector();
  v2 |= unstable_remove_if([](int) { return true; });
  assert(v2.size() == 0);

  auto v3 = make_vector();
  v3 |= unstable_remove_if([](int i) { return i == 2; });
  assert(v3.size() == 4);

  return 0;
}