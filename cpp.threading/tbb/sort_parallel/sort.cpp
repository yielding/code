#include <oneapi/tbb/parallel_sort.h>
#include <oneapi/tbb/tick_count.h>

#include <vector>
#include <print>

using namespace oneapi::tbb;
using namespace std;

const int TESTSIZ = 120 * 3000;

int main(int argc, char const* argv[])
{
  srand((unsigned)time(nullptr));

  vector<int> vi(TESTSIZ);
  for (int i=0; i<TESTSIZ; ++i) vi[i] = rand();

  tick_count t0, t1;

  for (int i=0; i<10; ++i)
  {
    vector<int> vi_clone1(vi), vi_clone2(vi);

    t0 = tick_count::now();
    sort(vi_clone1.begin(), vi_clone1.end());
    t1 = tick_count::now();

    println("std::sort {} ms", (t1 - t0).seconds());

    t0 = tick_count::now();
    parallel_sort(vi_clone2.begin(), vi_clone2.end());
    t1 = tick_count::now();

    println("tbb::parallel_sort {} ms", (t1 - t0).seconds());
  }

  return 0;
}
