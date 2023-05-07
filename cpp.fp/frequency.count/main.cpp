#include <iostream>
#include <algorithm>
#include <string>
#include <cctype>
#include <format>

#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace as = ranges::actions;
namespace rg = ranges;

using namespace std;
using rg::to;

int main(int argc, char* argv[])
{
  auto const& s = string("Radar");
  auto lowered = s 
    | rv::transform([](unsigned char c) { return tolower(c); }) 
    | to<string>
    | as::sort;

  cout << lowered << endl;

  auto grouped = lowered 
    | rv::chunk_by([](char x, char y) { return x == y; });

  cout << grouped << endl;

  auto count = grouped 
    | rv::transform([](auto &&r) { return rg::distance(r); });

  cout << rv::all(count) << endl;

  auto uniqs = lowered | rv::unique;
  auto rng = rv::zip(uniqs, count);

  for (auto const& e: rng)
    cout << format("({}:{})", e.first, e.second) << endl;

  return 0;
}