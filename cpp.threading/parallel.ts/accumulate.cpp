#include <iostream>
#include <exception>
#include <chrono>
#include <vector>
#include <numeric>
#include <oneapi/dpl/execution>
#include <oneapi/dpl/algorithm>
#include <oneapi/dpl/numeric>
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
    auto policy = execution::par;
    auto t1 = chrono::high_resolution_clock::now();
    auto result = reduce(policy, v.begin(), v.end());
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    println("std::reduce result {0} took {1} ms", result, ms.count());
  }

  {
    auto policy = oneapi::dpl::execution::par;
    auto t1 = chrono::high_resolution_clock::now();
    auto result = reduce(policy, v.begin(), v.end());
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    println("std::reduce result {0} took {1} ms", result, ms.count());
  }

  return 0;
}