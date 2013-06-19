#include <iostream>
#include <vector>
#include <set>
#include <numeric>
#include <chrono>

using namespace std;
using namespace std::chrono;

template <typename C> 
int64_t sum(C& c)
{
  int64_t sum = 0;
  for (auto it=c.begin(); it!=c.end(); ++it)
    sum += *it;

  return sum;
}

int main(int argc, char const* argv[])
{
  int const COUNT = 1000000;

  vector<int64_t> v(COUNT);
  set<int64_t> s;

  for (int64_t i=0; i<COUNT; i++) v[i] = i;
  for (int64_t i=0; i<COUNT; i++) s.insert(i);

  // sum using set
  {
  auto start = system_clock::now();
  cout << "sum : " << sum(s) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }

  // sum using vector
  {
  auto start = system_clock::now();
  cout << "sum : " << sum(v) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }

  auto x = [=](int64_t so_far, int64_t v) { return so_far + v; };
  // accumulate set
  {
  auto start = system_clock::now();
  cout << "sum : " << accumulate(s.begin(), s.end(), 0LL, x) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }

  // accumulate vector
  {
  auto start = system_clock::now();
  cout << "sum : " << accumulate(v.begin(), v.end(), 0LL, x) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }
  
  return 0;
}
