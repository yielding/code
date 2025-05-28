#include <exception>
#include <chrono>
#include <vector>
#include <numeric>
#include <oneapi/dpl/execution>
#include <oneapi/dpl/algorithm>
#include <oneapi/dpl/numeric>
#include <print>

using namespace std;

template<typename F>
void executor(F f)
{
  auto t1 = chrono::high_resolution_clock::now();
  auto result = f();
  auto t2 = chrono::high_resolution_clock::now();
  chrono::duration<double, milli> ms = t2 - t1;
  println("std::accumulation result {0} took {1} ms", result, ms.count());
}

int main()
{
  vector v(1'000'000'007, 0.5);

  executor([&v] { return accumulate(v.begin(), v.end(), 0.0); });

  executor([&v] { return reduce(execution::par, v.begin(), v.end()); });

  executor([&v] { return reduce(dpl::execution::par, v.begin(), v.end()); });

  return 0;
}