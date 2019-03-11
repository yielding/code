#include <chrono>
#include <ctime>
#include <iostream>
#include <iomanip>
#include "date.h"

using namespace std;
using namespace date;

int main(int argc, char const* argv[])
{
  // 1. time_point
  auto tp = chrono::system_clock::now();
  cout << tp << endl;

  // 2. year-month-day
  auto dp  = floor<days>(tp);
  auto ymd = year_month_day{dp};
  cout << ymd << endl;

  auto time = make_time(tp-dp);
  cout << time << endl;

  return 0;
}
