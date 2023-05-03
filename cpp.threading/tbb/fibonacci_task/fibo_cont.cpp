#include "tbb/tick_count.h"
#include "tbb/task.h"
#include "tbb/task_scheduler_init.h"
#include <boost/format.hpp>
#include <stdint.h>
#include <cstdlib>
#include <string>
#include <iostream>

using namespace std;
using namespace tbb;
using namespace boost;

int64_t serial_fib(int64_t n)
{
  return n < 2 ? n : serial_fib(n-1) + serial_fib(n-2);
}

struct fib_continuation: task
{
  int64_t* const sum;
  int64_t x, y;
  fib_continuation(int64_t* sum_): sum(sum_)
  {}

  task* execute()
  {
    *sum = x + y;
    return NULL;
  }
};

class fib_task: public task
{
public:
  fib_task(int64_t n, int64_t* sum)
    :m_n(n), m_sum(sum)
  {}

  task* execute()
  {
    if (m_n < 22)
    {
      *m_sum = serial_fib(m_n);
    }
    else
    {
      fib_continuation& c = *new(allocate_continuation()) fib_continuation(m_sum);

      fib_task& a = *new(c.allocate_child()) fib_task(m_n - 2, &c.x);
      fib_task& b = *new(c.allocate_child()) fib_task(m_n - 1, &c.y);

      c.set_ref_count(2);
      spawn(b);
      spawn(a);
    }

    return NULL;
  }

private:
  int64_t  const m_n;
  int64_t* const m_sum;
};

int64_t parallel_fib(int64_t n)
{
  int64_t sum;
  fib_task& a = *new(task::allocate_root())fib_task(n, &sum);
  task::spawn_root_and_wait(a);
  return sum;
}

int main(int argc, char const* argv[])
{
  // int nthread = task_scheduler_init::automatic;

  task_scheduler_init init(10);

  cout << "thread no: " << 10 << endl;
  tick_count t0 = tick_count::now();
  int n = strtol(argv[2], 0, 0);
  if (string(argv[1]) == "s") 
      cout << str(format("serial %d: %d\n") % n % serial_fib(n));
  else
      cout << str(format("parallel %d: %d\n") % n % parallel_fib(n));
  tick_count t1 = tick_count::now();

  cout << str(format("elapsed: %d seconds\n") % (t1-t0).seconds());

  return 0;
}
