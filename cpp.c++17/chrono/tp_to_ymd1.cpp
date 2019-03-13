#include <chrono>
#include <ctime>
#include <iostream>

using namespace std;
using namespace std::chrono;

int main()
{
  typedef duration<int, ratio_multiply<hours::period, ratio<24> >::type> days;
  auto now = system_clock::now();
  auto tp  = now.time_since_epoch();  // duration
  auto d   = duration_cast<days>(tp); // day
  tp -= d;
  hours h = duration_cast<hours>(tp);
  tp -= h;
  minutes m = duration_cast<minutes>(tp);
  tp -= m;
  seconds s = duration_cast<seconds>(tp);
  tp -= s;
  cout << d.count() << "d " << h.count() << ':'
       << m.count() << ':' << s.count();

  cout << " " << tp.count() << "["
       << system_clock::duration::period::num << '/'
       << system_clock::duration::period::den << "]\n";

  auto tt = system_clock::to_time_t(now);
  tm utc_tm = *gmtime(&tt);
  tm local_tm = *localtime(&tt);

  cout << utc_tm.tm_year + 1900 << '-';
  cout << utc_tm.tm_mon + 1 << '-';
  cout << utc_tm.tm_mday << ' ';
  cout << utc_tm.tm_hour << ':';
  cout << utc_tm.tm_min  << ':';
  cout << utc_tm.tm_sec  << '\n';

  return 0;
}
