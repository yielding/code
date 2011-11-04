#include <boost/chrono/chrono.hpp>
#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;
using namespace boost::chrono;

int* buffer;
uint64_t const size = 10000000ul;

void test(vector<uint64_t>& v)
{
  auto t0 = system_clock::now();
  for (uint64_t i=0; i<size; ++i)
    v.push_back(buffer[i]);
    // copy(buffer, buffer+size, back_inserter(v));

  auto t1 = system_clock::now();
  cout << "elapsed: " << (t1 - t0).count();
  cout << "size: " << v.size() << endl;
}

void test_assign(vector<uint64_t>& v)
{
  auto t0 = system_clock::now();
  v.assign(buffer, buffer+size);

  auto t1 = system_clock::now();
  cout << "elapsed: " << (t1 - t0).count();
  cout << "size: " << v.size() << endl;
}

void alloc()
{
  auto t0 = system_clock::now();

  buffer = new int[size];
  for (uint64_t i=0; i<size; i++)
    buffer[i] = i;

  auto t1 = system_clock::now();
  cout << "alloc elapsed: " << (t1 - t0).count() << endl;
}

int main(int argc, char const* argv[])
{
  alloc();

  vector<uint64_t> v2;
  test_assign(v2);


  vector<uint64_t> v;
  v.reserve(size);
  test(v);

  vector<uint64_t> v1;
  test(v1);

  delete [] buffer;

  return 0;
}
