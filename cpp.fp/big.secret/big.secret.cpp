#include <iostream>
#include <vector>
#include <format>
#include <range/v3/all.hpp> 

namespace rv = ranges::views;
namespace rg = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  auto k = 4;

  auto const target = string{"IHAVEABIGSECRET"}; 
  auto init = rv::iota(0, k-1);
  auto reversed = rv::iota(1, k) | rv::reverse;
  auto combined = rv::concat(init, reversed);
  auto repeat = combined | rv::cycle;
  // NOTICE
  // the power of zip (with free type!)
  auto pair_with_target 
    = rv::zip(repeat, target)
    | rg::to<vector<pair<int, char>>>;

  rg::stable_sort(pair_with_target, 
      [](auto const& p1, auto const& p2) {
        return p1.first < p2.first;
      });

  auto message = pair_with_target | rv::values;
  cout << rv::all(message) << endl;

  return 0;
}