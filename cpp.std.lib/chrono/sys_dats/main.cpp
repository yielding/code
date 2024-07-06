#include <chrono>
#include <iostream>

using namespace std::chrono;
using namespace std;

int main(int argc, char* argv[])
{
  year_month_day _ymd = 2023y/11/9;
  auto tp = sys_days{_ymd} + hours(5) + minutes(30) + seconds(10);

  cout << tp << endl;

  return 0;
}
