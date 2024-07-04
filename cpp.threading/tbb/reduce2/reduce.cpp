#include <execution>

#include <chrono>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <utility>
#include <vector>

using namespace std;

int main()
{
  cout << fixed << setprecision(1);

  auto eval = [](auto fun)
  {
    const auto t1 = chrono::high_resolution_clock::now();
    const auto [name, result] = fun();
    const auto t2 = chrono::high_resolution_clock::now();
    const chrono::duration<double, milli> ms = t2 - t1;
    cout << setw(28) << left << name << "sum: "
         << result << '\t' << "time: " << ms.count() << " ms\n";
  };

  {
    const vector<double> v(100'000'007, 0.1);

    eval([&v]{ return pair{"accumulate (double)",  accumulate(v.cbegin(), v.cend(), 0.0)}; });
    eval([&v]{ return pair{"reduce (seq, double)", reduce(execution::seq, v.cbegin(), v.cend())}; });
    eval([&v]{ return pair{"reduce (par, double)", reduce(execution::par, v.cbegin(), v.cend())}; });
  }

  {
    const vector<long> v(100'000'007, 1);

    eval([&v]{ return pair{"accumulate (long)",  accumulate(v.cbegin(), v.cend(), 0l)}; });
    eval([&v]{ return pair{"reduce (seq, long)", reduce(execution::seq, v.cbegin(), v.cend())}; });
    eval([&v]{ return pair{"reduce (par, long)", reduce(execution::par, v.cbegin(), v.cend())}; });
  }
}