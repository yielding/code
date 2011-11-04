#include <boost/chrono/chrono.hpp>
#include <iostream>
#include <vector>

using namespace std;
using namespace boost::chrono;

void test(vector<uint64_t>& v)
{
  uint64_t const size = 10000000ul;

  auto t0 = system_clock::now();
  for (uint64_t i=0; i<size; ++i)
    v.push_back(i);

  auto t1 = system_clock::now();
  cout << "elapsed: " << (t1 - t0).count() << endl;
}

int main(int argc, char const* argv[])
{
  uint64_t const size = 10000000ul;

  for(size_t i=0; i<100000;i++)
    ;

  vector<uint64_t> v1;
  test(v1);

  vector<uint64_t> v;
  v.reserve(size);
  test(v);

  return 0;
}
