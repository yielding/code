#include <iostream>
#include <vector>
#include <string>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

template <typename Range>
void write_top_k(const Range& xs, int count)
{
  auto print = [](const auto& p) { return to_string(p.second) + " " + p.first; };

  auto items = view::zip(xs, view::ints(1))
             | view::transform(print)
             | view::take(5);

  for (auto const& item: items)
    cout << item << "\n";
}

template <typename Range>
auto top_k(const Range& xs, int count) -> vector<pair<long,string>>
{
  auto& copy = ::ranges::copy;

  auto r0 = xs | copy | actions::sort;
  auto r1 = r0 
    | view::chunk_by(std::equal_to<>())
    | view::transform([](const auto& group) {
        auto b = std::begin(group);
        auto e = std::end(group);
        return make_pair(std::distance(b, e), *b);
      })
    | to_vector | action::sort;

  return r1 
    | view::reverse | view::take(count)
    | to<vector>();
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