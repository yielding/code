#include <chrono>

int main()
{
  using namespace std::chrono;

  auto tp = zoned_time{current_zone(), system_clock::now()}.get_local_time();
  auto dp = floor<days>(tp);
  year_month_day ymd{dp};
  hh_mm_ss time{floor<milliseconds>(tp-dp)};
  auto y = ymd.year();
  auto m = ymd.month();
  auto d = ymd.day();
  auto h = time.hours();
  auto M = time.minutes();
  auto s = time.seconds();
  auto ms = time.subseconds();

  return 0;
}
