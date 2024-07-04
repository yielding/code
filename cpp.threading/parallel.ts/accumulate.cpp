#include <iostream>
#include <chrono>
#include <vector>
#include <numeric>
#include <execution>
#include <print>

using namespace std;

int main()
{
  vector v(10'000'007, 0.5);

  {
    auto t1 = chrono::high_resolution_clock::now();
    auto result = accumulate(v.begin(), v.end(), 0.0);
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    println("std::accumulate result {0} took {1} ms", result, ms.count());
  }

  {
    auto t1 = chrono::high_resolution_clock::now();
    auto result = reduce(execution::par, v.begin(), v.end());
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    println("std::reduce result {0} took {1} ms", result, ms.count());
  }

  return 0;
}