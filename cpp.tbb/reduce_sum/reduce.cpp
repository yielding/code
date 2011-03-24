#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_reduce.h"
#include "tbb/tick_count.h"


#include <boost/thread/mutex.hpp>
#include <algorithm>
#include <iostream>

using namespace std;
using namespace tbb;

boost::mutex io_mutex;

float Foo(float a)
{
  for (size_t i=0; i<100; i++)
  {
    ;
  }
  return a;
}

class SumFoo
{
public:
  SumFoo(float* arr) 
    : m_arr(arr)
    , m_sum(0.0)
  {}

  SumFoo(SumFoo const& x, split)
    : m_arr(x.m_arr), m_sum(0)
  {}

  void join(SumFoo const& y)
  {
    m_sum += y.m_sum;
  }

  void operator() (blocked_range<size_t> const& r)
  {
    float* arr = m_arr;
    float  sum = m_sum;
    size_t end = r.end();
    for (size_t i=r.begin(); i!=end; ++i)
      sum += Foo(arr[i]);

    m_sum = sum;
  }

  float sum()
  {
    return m_sum;
  }

private:
  float* m_arr;

public:
  float m_sum;
};

float ParallelSumFoo(float a[], size_t n)
{
  SumFoo sf(a);
  parallel_reduce(blocked_range<size_t>(0, n), sf);
  return sf.sum();
}

int main(int argc, char const* argv[])
{
  task_scheduler_init init;
  int const N = 10000;
  float a[N];

  for (size_t i=0; i<N; i++) a[i] = 1.0;

  tbb::tick_count t0, t1;

  t0 = tbb::tick_count::now();
  float res = ParallelSumFoo(a, N);
  t1 = tbb::tick_count::now();
  cout << "time: " << (t1-t0).seconds() * 1000 << " result: " << res << endl;

  t0 = tbb::tick_count::now();
  float sum = 0.0;
  for (size_t i=0; i<N; i++)
    sum += Foo(a[i]);
  t1 = tbb::tick_count::now();

  cout << "time: " << (t1-t0).seconds()  * 1000 << " result: " << sum << endl;
  
  return 0;
}


