#include <vector>
#include <cassert>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;

int main(int argc, char *argv[])
{
  using a::take, g::to, v::ints;

  auto v0 = ints(1, 21) | to<vector>;
  auto&v1 = take(v0, 17);

  assert(&v0 == &v1);
  assert(v0.size() == 17u);
  assert(v1.size() == 17u);
  assert(v0.back() == 17);

  v0 = std::move(v0) | take(14);
  assert(v0.size() == 14u);
  assert(v0.back() == 14);

  v0 |= take(11);
  assert(v0.size() == 11u);
  assert(v0.back() == 11);

  v0 |= take(100);
  assert(v0.size() == 11u);
  assert(v0.back() == 11);

  v0 |= take(0);
  assert(v0.size() == 0u);
  
  return 0;
}