#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int agc, char* agv[])
{
  auto const v1 = vector{ 2,  3,  7};
  auto const v2 = vector{11, 17, 21};
  cout << v::concat(v1, v2) << endl;

  auto const v3 = v::concat(v::single(10), v2);
  cout << v3 << endl;

  auto const v4 = vector<vector<int>> {{1, 2}, {7}, {3, 5}};
  auto rng = v4 | v::join;
  cout << rng << endl;

  return 0;
}
