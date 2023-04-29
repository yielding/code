#include <algorithm>
#include <chrono>
#include <execution>
#include <iostream>
#include <random>
#include <vector>
#include <cassert>

using namespace std;

using clk = chrono::high_resolution_clock;

int main() 
{
  // Size of our vector
  const int N = 10000000;
  decltype(clk::now()) start, finish;

  // Create our vector
  vector<int> v0(N);

  // Set up our random number generator
  mt19937 rng;
  rng.seed(random_device()());
  uniform_int_distribution<int> dist(0, 255);

  // Generate our random inputs
  generate(begin(v0), end(v0), [&]() { return dist(rng); });

  // Reduce the vector in parallel and vectorized
  start = clk::now();
  sort(execution::par, begin(v0), end(v0));
  finish = clk::now();

  // Print out the execution time
  auto duration =
      chrono::duration_cast<chrono::milliseconds>(finish - start);
  cout << "parallel elapsed time = " << duration.count() << " ms\n";

  auto v1 = v0;
  // Reduce the vector in parallel and vectorized
  start = clk::now();
  sort(execution::seq, begin(v1), end(v1));
  finish = clk::now();

  // Print out the execution time
  duration = chrono::duration_cast<chrono::milliseconds>(finish - start);
  cout << "sequence elapsed time = " << duration.count() << " ms\n";

  assert(v0 == v1);

  return 0;
}