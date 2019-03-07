#include <chrono>
#include <ctime>
#include <iostream>

using namespace std;
using namespace std::chrono;

void print_time(system_clock::time_point t)
{
  auto c_time = system_clock::to_time_t(t);
  auto  tmptr = localtime(&c_time);
  system_clock::duration d = t.time_since_epoch();

  cout << tmptr->tm_hour << ':' 
       << tmptr->tm_min  << ':' 
       << tmptr->tm_sec  << '.' 
       << (d - duration_cast<seconds>(d)).count();

}

int main(int argc, char const* argv[])
{
  //auto t = system_clock::now();
  //print_time(t);
  auto unix_ts = chrono::seconds(time(nullptr)).count();
  cout << unix_ts << endl;
  cout << chrono::milliseconds(unix_ts).count() << endl;


  return 0;
}
