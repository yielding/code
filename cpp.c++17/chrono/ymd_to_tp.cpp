#include "date.h"
#include <iostream>

using namespace std;
using namespace std::chrono;
using namespace date;

/*
int main()
{
  // convert system_clock::time_point to days-precision time_point
  auto sd = floor<days>(t);

  // create time_of_day
  auto tod = make_time(t - sd);

  // create year_month_day
  year_month_day ymd = sd;

  // extract field types as int
  int y = int{ymd.year()}; // Note 1
  int m = unsigned{ymd.month()};
  int d = unsigned{ymd.day()};
  int h = tod.hours().count();
  int M = tod.minutes().count();
  int s = tod.seconds().count();
  int us = duration_cast<microseconds>(tod.subseconds()).count();

  return 0;
}
*/

int main(int argc, char *argv[])
{
  int y = 2019;
  int m = 3;
  int d = 13;
  int h = 15;
  int M = 33;
  int s = 55;
  int us = 123456;
  auto t = sys_days(year{y}/m/d) + hours{h} + minutes{M} + seconds{s} + microseconds{us};

  cout << t;
  
  return 0;
}
