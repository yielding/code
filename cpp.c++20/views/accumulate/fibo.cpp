#include <iostream>
#include <range/v3/all.hpp>

using namespace std;
using namespace ranges;

int main()
{   
  auto rng = views::generate(
    [p=pair{0, 1}]() mutable {
      auto [a0, b0] = p; 
      p = {b0, a0 + b0};
      return a0;
    }
  ); 

  auto fib10 = rng | views::take(10); // [0,1,1,2,3,5,8,13,21,34]
  cout << fib10 << endl;

  //cout << ranges::accumulate(fib10, 0);
  auto res = ranges::accumulate(views::ints(1, 10) 
           | views::transform([](int i) { return i*i; }), 0);
  cout << res;

  return 0;
}
