#include <chrono>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <sstream>
#include "date.h"

using namespace std;
using namespace date;

int main(int argc, char const* argv[])
{
  // 1. time_point
  auto tp = chrono::system_clock::now();
  cout << tp << endl;

  // 1.2
  stringstream s;
  s << tp;

  cout << s.str() << endl;

  // 2. year-month-day
  auto dp  = floor<days>(tp);
  auto ymd = year_month_day{dp};
  cout << ymd << endl;

  // 3. hour-minutes-seconds
  auto t0 = make_time(tp - dp);
  cout << t0 << endl;
  cout << t0.hours() << endl;

  // 4. unix timestamp
  time_t t1 = 1552287917;
  auto sys_time = chrono::system_clock::from_time_t(t1);
  cout << sys_time << endl;

  return 0;
}