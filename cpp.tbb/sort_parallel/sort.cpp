#include "tbb/task_scheduler_init.h"
#include "tbb/parallel_sort.h"
#include "tbb/tick_count.h"
#include <vector>
#include <iostream>

using namespace std;
using namespace tbb;

int main(int argc, char const* argv[])
{
  srand((unsigned)time(NULL));

  tbb::task_scheduler_init();
  const int TESTSIZ = 120 * 3000;

  std::vector<int> vi(TESTSIZ);
  for (int i=0; i<TESTSIZ; ++i) vi[i] = rand();

  for (int i=0; i<10; ++i)
  {
    std::vector<int> vi_clone1(vi);
    std::vector<int> vi_clone2(vi);

    tick_count t0, t1;
    t0 = tick_count::now();
    std::sort(vi_clone1.begin(), vi_clone1.end());
    t1 = tick_count::now();
    cout << "std::sort - " << (t1-t0).seconds() << " ms" << endl;

    t0 = tick_count::now();
    tbb::parallel_sort(vi_clone2.begin(), vi_clone2.end());
    t1 = tick_count::now();
    cout << "tbb::parallel_sort - " << (t1-t0).seconds() << " ms" << endl;
  }

  return 0;
}
