#include <iostream>
#include <vector>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/for_each.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/repeat_n.hpp>

using namespace ranges;
using namespace std;

int main()
{
  auto vi = views::for_each(views::ints(1, 6), 
                            [](int i) { return yield_from(views::repeat_n(i, i)); }) 
              | to<vector>();

  cout << views::all(vi) << '\n';
  // prints: [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]

  return 0;
}