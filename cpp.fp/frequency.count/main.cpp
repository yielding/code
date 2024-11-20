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

auto frequency_cout(string const& input) -> vector<pair<char, long>>
{
  auto lowered = input 
    | v::transform([](unsigned char c) { return tolower(c); }) 
    | g::to<string> | a::sort;                               // "aadrr"

  cout << lowered << endl;

  auto grouped = lowered 
    | v::chunk_by([](char x, char y) { return x == y; });    // [[a, a], [d], [r, r]]

  cout << grouped << endl;

  auto count = grouped 
    | v::transform([](auto &&r) { return g::distance(r); }); // [2, 1, 2]

  cout << v::all(count) << endl;

  auto uniqs = lowered | v::unique;
  return v::zip(uniqs, count) | g::to<vector>;               // [(a, 2), (d, 1), (r, 2)]
}

int main(int agc, char* agv[])
{
  auto rng = frequency_cout(string("Radar"));

  for (auto const& e: rng)
    cout << format("({}:{})", (char)e.first, e.second) << endl;

  return 0;
}