#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>
#include "parallel_regex.h"

using namespace std;
using namespace tbb;

int N = 1024*1024*1024;

void parallel_regex()
{
  tbb::tick_count t0 = tbb::tick_count::now();
  std::string fn = "test_1g.bin";
  matches res;
  RegexTask& a = *new(task::allocate_root())RegexTask(fn, 0, N, res);
  task::spawn_root_and_wait(a);
  tbb::tick_count t1 = tbb::tick_count::now();
  cout << res.size() << " : " << (t1 - t0).seconds() << endl;
  cout << res[1].offset << endl;
}

int main(int argc, char const* argv[])
{
  int nthread = task_scheduler_init::automatic;
  task_scheduler_init init(nthread*2);

  parallel_regex();

  return 0;
}
