//#include <iostream>
//#include <algorithm>
//#include <vector>
//#include <ranges>
//#include <cassert>
//
//using namespace std;
//using namespace ranges;
//
//struct sum
//{
//  void operator() (int n) { _sum += n; }
//  int _sum{0};
//};
//
//int main(int argc, char *argv[])
//{
//  vector<int> nums{3, 4, 2, 8, 15, 267};
//
//  auto print = [](const auto& n) { cout << ' ' << n; };
//
//  namespace ranges = std::ranges;
//
//  ranges::for_each(as_const(nums), print);
//  cout << "\n";
//
//  ranges::for_each(nums, [](int& n) {++n;});
//  ranges::for_each(as_const(nums), print);
//  cout << "\n";
//
//  auto [i, s] = ranges::for_each(nums.begin(), nums.end(), sum());
//  assert(i == nums.end());
//
//  ranges::for_each(nums.cbegin(), nums.cend(), print);
//
//  cout << "\n" << "sum : " << s._sum << "\n";
//
//  vector<int> v = {7,6,4,1,2,3,4,5};
//  ranges::views::all_t
//  cout << ranges::views::all(v) <<endl;
//
//
//  // auto vi =
//  //   views::for_each(views::ints(1, 10), [](int i) {
//  //       return yield_from(views::repeat_n(i, i));
//  //       })
//  // | to<std::vector>();
//
//
//  return 0;
//}

#include <iostream>
#include <vector>
 
#include <range/v3/range/conversion.hpp>
#include <range/v3/view/for_each.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/repeat_n.hpp>

using std::cout;
 
int main()
{
    using namespace ranges;
    auto vi = views::for_each(views::ints(1, 6),
                              [](int i) { return yield_from(views::repeat_n(i, i)); }) |
              to<std::vector>();
    // prints: [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]
    cout << views::all(vi) << '\n';
}
