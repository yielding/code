#include <boost/chrono/chrono.hpp>

#include <iostream>
#include <ctime>

using namespace std;
using namespace boost::chrono;

void print_time(system_clock::time_point t)
{
  auto c_time = system_clock::to_time_t(t);
  auto  tmptr = std::localtime(&c_time);
  system_clock::duration d = t.time_since_epoch();

  cout << tmptr->tm_hour << ':' 
       << tmptr->tm_min  << ':' 
       << tmptr->tm_sec  << '.' 
       << (d - duration_cast<seconds>(d)).count();
}

int main(int argc, char const* argv[])
{
  auto t = system_clock::now();
  print_time(t);

  return 0;
}
