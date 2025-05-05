#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int agc, char* agv[])
{
  using v::concat, v::single, v::join;

  auto v1 = { 2,  3,  7};
  auto v2 = {11, 17, 21};
  cout << concat(v1, v2) << endl;

  // single: prefix a range with an element
  // single: 상수를 concat 연산을 위해 list로 변환 (List monad의 identity)
  auto v3 = concat(single(10), v2);
  cout << v3 << endl;

  auto v4 = vector<vector<int>> {{1, 2}, {7}, {3, 5}};
  auto rng = v4 | join;
  cout << rng << endl;

  return 0;
}
