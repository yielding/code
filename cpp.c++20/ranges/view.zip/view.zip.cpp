#include <iostream>
#include <vector>
#include <string>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

template <typename Range>
void write_top_5(const Range& xs)
{
  auto print = [](const auto& p) { return to_string(p.second) + " " + p.first; };

  auto items = view::zip(xs, view::ints(1))
             | view::transform(print)
             | view::take(5);

  for (auto const& item: items)
    cout << item << "\n";
}

int main(int argc, char* argv[])
{
  vector<string> vs { "f", "g", "h", "a", "b", "1", "2", "3", "5", "ll"};

  vs |= action::sort;

  write_top_5(vs);

  return 0;
}