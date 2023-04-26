#include <iostream>
#include <range/v3/view/take.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/view/generate.hpp>
#include <range/v3/numeric/accumulate.hpp>
//#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char** argv)
{   
  auto rng = view::generate(
    [p=pair{1, 1}]() mutable {
      auto [a0, b0] = p; 
      p = {b0, a0 + b0};
      return a0;
    }
  ); 

  auto fib10 = rng | view::take(10);
  cout << fib10 << endl;

  auto rng2 = view::ints(1, 10) | 
              view::transform([](int i) { return i*i; });

  cout << acc/Users/yielding/Documents/cs/cs.os/os.multiplexing/kqueue-echo.c umulate(rng2 , 0);

  return 0;
}