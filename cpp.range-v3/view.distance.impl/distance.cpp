#include <iostream>
#include <vector>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int distance_impl(string const& s)
{
  auto r = s | v::transform([](char c) { return 1; });

  return g::accumulate(r, 0);
}

int main(int argc, char* argv[])
{
  auto s = "unparagoned"s;
  auto l = distance_impl(s);

  assert(s.length() == l);

  return 0;
}