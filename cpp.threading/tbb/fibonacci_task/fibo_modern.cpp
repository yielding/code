#include <tbb/task_group.h>
#include <tbb/tick_count.h>
#include <iostream>
#include <cstdint>
#include <cstdlib>
#include <string>

using namespace std;
using namespace tbb;

int64_t serial_fib(const int64_t n)
{
  return n < 2 ? n : serial_fib(n-1) + serial_fib(n-2);
}

int64_t parallel_fib(const int64_t n)
{
  if (n < 22)
    return serial_fib(n);

  int64_t x, y;
  task_group g;

  g.run([&] { x = parallel_fib(n - 1); });
  g.run([&] { y = parallel_fib(n - 2); });
  g.wait();

  return x + y;
}

int main(const int argc, char const* argv[])
{
  if (argc != 3)
  {
    cerr << "Usage: " << argv[0] << " <s|p> <number>\n";
    return 1;
  }

  auto t0 = tick_count::now();
  auto n = strtol(argv[2], nullptr, 0);

  if (string(argv[1]) == "s")
    cout << "serial " << n << ": " << serial_fib(n) << "\n";
  else
    cout << "parallel " << n << ": " << parallel_fib(n) << "\n";

  auto t1 = tick_count::now();
  cout << "elapsed: " << (t1-t0).seconds() << " seconds\n";

  return 0;
}