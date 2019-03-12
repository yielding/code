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
  sys_days dp = 2015_y/sep/25;
  cout << dp.time_since_epoch().count() << endl;

  auto tp = sys_days{jan/3/1970} + 7h + 33min + 20s;
  cout << tp.time_since_epoch() << endl;

  return 0;
}
