#include <functional>
#include <iostream>
#include <string>
#include <vector>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std; 

template <typename Range>
void write_top_10(const Range& xs)
{

  auto to_s = [](const auto& p) { return to_string(p.second) + " " + p.first; };

  auto items = v::zip(xs, v::ints(1, g::unreachable))
    | v::transform(to_s)
    | v::take(10);

  for (const auto& item: items) {
    cout << item << endl;
  }
}

int main(int argc, char *argv[])
{
  auto movies_1 = vector{
    "Meaning of life"s,
    "Dr Strangelove"s
  };

  // If there are less than 10 movies in the vector,
  // all will be printed.  If there are more than 10
  // movies in the vector, only the first 10 will be printed
  write_top_10(movies_1);

  return 0;
}
