#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>
#include <boost/format.hpp>

#include "partial_regex.h"
#include "tbb/parallel_reduce.h"

#include "asio_threadpool.h"

using namespace std;
using namespace tbb;
using namespace boost;

int64_t N = 1024*1024*1024*16L;
std::string const fn = "test_16g.bin";

class ReduceRegex
{
public:
  ReduceRegex(string const& fn)
    : m_fn(fn)
  {}

  ReduceRegex(ReduceRegex& x, split)
    : m_fn(x.m_fn), m_result(x.m_result)
  {
    copy(x.m_result.begin(), x.m_result.end(), back_inserter(m_result));
  }

  void operator() (const blocked_range<int64_t>& r)
  {
    std::ifstream in; in.open(m_fn.c_str(), ios_base::binary | ios_base::in);
    if (!in.is_open())
    {
      cout << "open error\n";
      return;
    }

    int64_t beg = r.begin();
    int64_t end = r.end();

    PartialRegex regex(beg, end - beg);
    in.seekg(beg, ios_base::beg);
    if (regex.search(boost::regex("\xff\xff"), in))
    {
      if (regex.result().size() > 0)
      {
        matches const& r = regex.result();
        copy(r.begin(), r.end(), back_inserter(m_result));
      }
    }
  }

  void join(ReduceRegex const& y)
  {
    copy(y.m_result.begin(), y.m_result.end(), back_inserter(m_result));
  }

  string m_fn;
  matches m_result;
};


//void parallel_regex()
//{
//  tbb::tick_count t0 = tbb::tick_count::now();
//
//  ReduceRegex rr("test_16g.bin");
//  parallel_reduce(blocked_range<int64_t>(0, N), rr);
//  matches res = rr.m_result;
//
//  tbb::tick_count t1 = tbb::tick_count::now();
//  cout << res.size() << " : " << (t1 - t0).seconds() << endl;
//  cout << res[1].offset << endl;
//}

void go(int64_t beg, int64_t end)
{
    std::ifstream in; in.open(fn.c_str(), ios_base::binary | ios_base::in);
    if (!in.is_open())
    {
      cout << "open error\n";
      return;
    }

    PartialRegex regex(beg, end - beg);
    in.seekg(beg, ios_base::beg);
    if (regex.search(boost::regex("\xff\xff"), in))
    {
      if (regex.result().size() > 0)
      {
        matches const& r = regex.result();
        cout << r.size() << endl;
      }
    }
}

void asio_regex()
{
  threadpool pool(5);

  pool.start();

  int64_t v0 = N / 4;
  int64_t v1 = N / 2;
  int64_t v2 = (N / 4) * 3;

  tbb::tick_count t0 = tbb::tick_count::now();
  pool.post(bind(go,  0, v0));
  pool.post(bind(go, v0, v1));
  pool.post(bind(go, v1, v2));
  pool.post(bind(go, v2, N));

  tbb::tick_count t1 = tbb::tick_count::now();
  cout << (t1 - t0).seconds() << endl;
}

void serial_regex()
{
  tbb::tick_count t0 = tbb::tick_count::now();
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
  if (regex.search(boost::regex("\xff\xff"), in))
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

  // serial_regex();
  // parallel_regex();

  asio_regex();

  return 0;
}
