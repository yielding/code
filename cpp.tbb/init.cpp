#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"

#include <boost/thread/mutex.hpp>
#include <iostream>

using namespace std;
using namespace tbb;

boost::mutex io_mutex;

void Foo(float a)
{
  boost::mutex::scoped_lock lk(io_mutex);
  a *= 5;
  cout << a << endl;
}

class ApplyFoo
{
public:
  ApplyFoo(float* arr) : m_arr(arr)
  {}

  void operator()(blocked_range<size_t> const& r) const
  {
    float* arr = m_arr;
    for (size_t i=r.begin(); i!=r.end(); ++i)
      Foo(arr[i]);
  }

private:
  float* m_arr;
};

void ParallelApplyFoo(float a[], size_t n)
{
  parallel_for(blocked_range<size_t>(0, n), ApplyFoo(a), auto_partitioner());
}

int main(int argc, char const* argv[])
{
  task_scheduler_init init;
  int const N = 10000;
  float a[N];

  for (size_t i=0; i<N; i++)
    a[i] = i * 1.0;
  
  ParallelApplyFoo(a, N);
  
  return 0;
}
