#include "tbb/tick_count.h"
#include "tbb/task.h"
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
      int64_t x, y;
      fib_task& a = *new(allocate_child()) fib_task(m_n - 1, &x);
      fib_task& b = *new(allocate_child()) fib_task(m_n - 2, &y);

      set_ref_count(3);
      spawn(b);
      spawn_and_wait_for_all(a);
      *m_sum = x + y;
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
  int nthread = task_scheduler_init::automatic;

  task_scheduler_init init(nthread);

  cout << "thread no: " << nthread << endl;
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
