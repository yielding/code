#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto chunk_scan(vector<int> arr, int n, auto f) -> vector<int>
{
  auto const scan = [&](auto&& r) {
    return g::views::partial_sum(r, f)
         | g::to<vector>;
  };

  return arr
    | v::chunk(n)
    | v::transform(scan)
    | v::join
    | g::to<vector>;
}

int main(int argc, char* argv[])
{
  auto vec = vector{1, 2, 3, 4, 5, 6};

  auto res = chunk_scan(vec, 2, plus{});

  cout << v::all(res) << endl;

  return 0;
}
