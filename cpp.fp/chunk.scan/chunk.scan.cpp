#include <iostream>
#include <format>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto chunk_scan(vector<int> arr, int n, auto f) -> vector<int>
{
  using v::chunk, v::transform, v::join, v::partial_sum, g::to;

  auto const scan = [&](auto&& r) {
    return partial_sum(r, f);
  };

  // DEBUG
  cout << v::all(arr | chunk(n) ) << endl;
  cout << v::all(arr | chunk(n) | transform(scan)) << endl;
  cout << v::all(arr | chunk(n) | transform(scan) | join) << endl;

  return arr
    | chunk(n)
    | transform(scan)
    | join
    | to<vector>;
}

int main(int argc, char* argv[])
{
  // REMARK
  // 1. 3을 기준으로 chunk를 나눈다 > [[1, 2, 3], [4, 5, 6]]
  // 2. 각 chunk를 partial_sum      > [[1, 3, 6], [4, 9, 15]]
  // 3. join                        > [1, 3, 6, 4, 9, 15]
  auto vec = vector{1, 2, 3, 4, 5, 6};
  auto res = chunk_scan(vec, 3, plus{});

  cout << v::all(res) << endl;

  string buffer;
  format_to(back_inserter(buffer), "Hello, C++{}!\n", "20");
  cout << buffer;

  return 0;
}
