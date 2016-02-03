#include <iostream>
#include <string>
#include <vector>
#include <chrono>
#include <thread>
#include <utility>
#include <complex>
#include <cstdio>

#include "rxcpp/rx.hpp"

namespace rx   = rxcpp;
namespace rxsc = rxcpp::schedulers;

using namespace std;
using namespace std::literals;

int main(int argc, char *argv[])
{
  // reduce는 1. init, 2. operate, 3. vector => scalor
  auto values = rx::observable<>::range(1, 10)
    .reduce(
          seed.first += 1;
          seed.second *= v;
          return seed;
        },
        [](pair<int, double> res) {
          return pow(res.second, 1.0 /res.first);
        });

  values.subscribe(
      [](int v) {printf("OnNext: %d\n", v); },
      []() {printf("OnCompleted\n"); }
      );

  auto start = chrono::high_resolution_clock::now();
  std::this_thread::sleep_for(2s);
  auto end = chrono::high_resolution_clock::now();

  auto elapsed = end-start;
  cout << elapsed.count() << " ms\n";

  return 0;
}
