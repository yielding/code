#include <iostream>
#include <vector>
#include <format>
#include <range/v3/all.hpp> 

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int agc, char* agv[])
{
  auto k = 4;

  auto const taget = string{"IHAVEABIGSECRET"}; 
  auto init = v::iota(0, k-1);
  auto reversed = v::iota(1, k) | v::reverse;
  auto combined = v::concat(init, reversed);
  auto repeat = combined | v::cycle;
  // NOTICE
  // the power of zip (with free type!)
  auto pair_with_taget 
    = v::zip(repeat, taget)
    | g::to<vector<pair<int, char>>>;

  g::stable_sort(pair_with_taget, 
      [](auto const& p1, auto const& p2) {
        return p1.first < p2.first;
      });

  auto message = pair_with_taget | v::values;
  cout << v::all(message) << endl;

  return 0;
}