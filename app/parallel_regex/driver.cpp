#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>
#include "parallel_regex.h"

using namespace std;
using namespace tbb;

int64_t N = 1024*1024*1024*16L;

void parallel_regex()
{
  tbb::tick_count t0 = tbb::tick_count::now();
  std::string fn = "test_16g.bin";
  matches res;

  RegexTask& a = *new(task::allocate_root())RegexTask(fn, 0, N, res);

  task::spawn_root_and_wait(a);
  tbb::tick_count t1 = tbb::tick_count::now();
  cout << res.size() << " : " << (t1 - t0).seconds() << endl;
  cout << res[1].offset << endl;
}

void serial_regex()
{
  tbb::tick_count t0 = tbb::tick_count::now();
  std::string fn = "test_16g.bin";
  matches res;

  std::ifstream in; in.open(fn.c_str(), ios_base::binary | ios_base::in);
  if (!in.is_open())
  {
    cout << "stream not opened\n";
    return;
  }
  else
  {
    cout << "ok 1\n";
  }

  PartialRegex regex(0, N);
  if (regex.search(boost::regex("\xff\xff"), in, false))
  {
    cout << "ok 2\n";
    res.swap(regex.m_results);
    tbb::tick_count t1 = tbb::tick_count::now();
    cout << res.size() << " : " << (t1 - t0).seconds() << endl;
    cout << res[1].offset << endl;
  }
}

int main(int argc, char const* argv[])
{
  int nthread = task_scheduler_init::automatic;
  task_scheduler_init init(nthread);

  serial_regex();
  // parallel_regex();

  return 0;
}
