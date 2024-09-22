#include <iostream>
#include <vector>
#include <string>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;

template <typename Range>
void write_top_k(const Range& xs, int count)
{
  auto print = [](const auto& p) { return to_string(p.second) + " " + p.first; };

  auto items = v::zip(xs, v::ints(1))
             | v::transform(print)
             | v::take(5);

  cout << v::all(items);
}

template <typename Range>
auto top_k(const Range& xs, int count) -> vector<pair<long,string>>
{
  using v::chunk_by, v::transform, v::reverse, v::take;
  using a::sort, g::copy, g::to;

  auto r0 = xs | copy | sort;
  auto r1 = r0 
    | chunk_by(equal_to{})
    | transform([](const auto& g) {
        auto b = begin(g);
        auto e = end(g);
        return make_pair(distance(b, e), *b);
      })
    | to<vector> | sort;

  return r1 | reverse | take(count) | to<vector>;
}

int main(int argc, char* argv[])
{
  auto vs = vector<string>{
    "f", "f", "kk", "kk", "g", "h", "a", "b", "1", 
    "2", "3", "5", "ll", "kk", "f"
  };

  auto&& r = top_k(vs, 3);
  for (auto& [f, s]: r) cout << f << " " << s << endl;

  return 0;
}