#include <iostream>
#include <algorithm>
#include <string>
#include <cctype>
#include <format>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;
using g::to;

int main(int agc, char* agv[])
{
  auto const& s = string("Radar");
  auto lowered = s 
    | v::transform([](unsigned char c) { return tolower(c); }) 
    | to<string>
    | a::sort;

  cout << lowered << endl;

  auto grouped = lowered 
    | v::chunk_by([](char x, char y) { return x == y; });

  cout << grouped << endl;

  auto count = grouped 
    | v::transform([](auto &&r) { return g::distance(r); });

  cout << v::all(count) << endl;

  auto uniqs = lowered | v::unique;
  auto rng = v::zip(uniqs, count);

  for (auto const& e: rng)
    cout << format("({}:{})\n", e.first, e.second);

  return 0;
}