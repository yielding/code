#include <chrono>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <sstream>

using namespace std::chrono;
using namespace std::literals;

int main(int argc, char const* argv[])
{
  sys_days dp = 2015y/September/25;
  std::cout << dp.time_since_epoch().count() << std::endl;

  auto tp = sys_days{January/3/1970} + 7h + 33min + 20s;
  std::cout << tp.time_since_epoch() << std::endl;

  return 0;
}