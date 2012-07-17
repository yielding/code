#include <iostream>
#include <vector>
#include <set>
#include <boost/chrono.hpp>

using namespace std;

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
  typedef boost::chrono::milliseconds ms;

  using namespace boost::chrono;

  int const COUNT = 10000000;
  vector<int64_t> v(COUNT);
  set<int64_t> s;

  // system_clock::time_point start = system_clock::now();

  for (int64_t i=0; i<COUNT; i++) v[i] = i;
  for (int64_t i=0; i<COUNT; i++) s.insert(i);

  {
  auto start = system_clock::now();
  cout << "sum : " << sum(s) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }

  {
  auto start = system_clock::now();
  cout << "sum : " << sum(v) << endl;
  auto stop  = system_clock::now();
  auto elapsed = (stop - start);
  cout << elapsed.count() << endl;
  }
  
  return 0;
}
