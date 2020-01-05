#include <iostream>
#include <chrono>
#include <vector>
#include <numeric>
#include <execution>

using namespace std;

int main()
{
  vector<double> v(10'000'007, 0.5);

  {
    auto t1 = chrono::high_resolution_clock::now();
    auto result = accumulate(v.begin(), v.end(), 0.0);
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    cout << std::fixed << "std::accumulate result " << result
         << " took " << ms.count() << " ms\n";
  }

  {
    auto t1 = chrono::high_resolution_clock::now();
    auto result = reduce(execution::par, v.begin(), v.end());
    auto t2 = chrono::high_resolution_clock::now();
    chrono::duration<double, milli> ms = t2 - t1;
    cout << "std::reduce result "
         << result << " took " << ms.count() << " ms\n";
  }
}
