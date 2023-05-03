#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

using namespace ranges;
using namespace std;

int main(int argc, char* argv[])
{
  auto make_vector = []() -> vector<int> { return {1, 2, 3, 4, 5}; };

  vector<int> v0;
  v0 |= actions::unstable_remove_if([](int) { return true; });
  assert(v0.empty());

  auto v1 = make_vector();
  v1 |= actions::unstable_remove_if([](int) { return false; });
  assert(v1.size() == 5);

  auto v2 = make_vector();
  v2 |= actions::unstable_remove_if([](int) { return true; });
  assert(v2.size() == 0);

  auto v3 = make_vector();
  v3 |= actions::unstable_remove_if([](int i) { return i == 2; });
  assert(v3.size() == 4);

  return 0;
}