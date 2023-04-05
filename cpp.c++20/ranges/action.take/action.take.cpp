#include <iostream>
#include <vector>
#include <cassert>
#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char *argv[])
{
  auto v0 = views::ints(1, 21) | to<vector>();
  auto&v1 = actions::take(v0, 17);

  assert(&v0 == &v1);
  assert(v0.size() == 17u);
  assert(v1.size() == 17u);
  assert(v0.back() == 17);

  v0 = std::move(v0) | actions::take(14);
  assert(v0.size() == 14u);
  assert(v0.back() == 14);

  v0 |= actions::take(11);
  assert(v0.size() == 11u);
  assert(v0.back() == 11);

  v0 |= action::take(100);
  assert(v0.size() == 11u);
  assert(v0.back() == 11);

  v0 |= action::take(0);
  assert(v0.size() == 0u);
  
  return 0;
}