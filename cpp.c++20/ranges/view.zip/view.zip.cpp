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
void write_top_k2(const Range& xs, int count)
{
  auto results = 
    xs | view::chunk_by(std::equal_to<>())
       | view::transform([](const auto& group) {
           auto b = std::begin(group);
           auto e = std::end(group);
           return make_pair(std::distance(b, e), *b);
         })
       | to_vector | action::sort;

  for (auto value: results | view::reverse | view::take(count))
    cout << value.first << " " << value.second << endl;
}

int main(int argc, char* argv[])
{
  vector<string> vs { "f", "f", "kk", "kk", "g", "h", "a", "b", "1", "2", "3", "5", "ll", "kk", "f"};

  vs |= action::sort;
  //write_top_k(vs, 5);
  write_top_k2(vs, 5);

  return 0;
}