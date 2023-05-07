#include <iostream>
#include <vector>
#include <string>

#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;
namespace as = ranges::actions;

using namespace std;

template <typename Range>
void write_top_k(const Range& xs, int count)
{
  auto print = [](const auto& p) { return to_string(p.second) + " " + p.first; };

  auto items = rv::zip(xs, rv::ints(1))
             | rv::transform(print)
             | rv::take(5);

  cout << rv::all(items);
}

template <typename Range>
auto top_k(const Range& xs, int count) -> vector<pair<long,string>>
{
  using rg::copy, as::sort, rg::to;

  auto r0 = xs | copy | sort;
  auto r1 = r0 
    | rv::chunk_by(equal_to<>())
    | rv::transform([](const auto& g) {
        auto b = begin(g);
        auto e = end(g);
        return make_pair(distance(b, e), *b);
      })
    | to<vector> | sort;

  return r1 
    | rv::reverse | rv::take(count) | to<vector>;
}

int main(int argc, char* argv[])
{
  vector<string> vs { 
    "f", "f", "kk", "kk", "g", "h", "a", "b", "1", 
    "2", "3", "5", "ll", "kk", "f"
  };

  auto&& r = top_k(vs, 3);
  for (auto const& value: r)
    cout << value.first << " " << value.second << endl;

  return 0;
}