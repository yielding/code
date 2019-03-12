#include "date.h"
#include <iostream>

// Reduce verbosity but let you know what is in what namespace
namespace C = std::chrono;
namespace D = date;
namespace S = std;

int main()
{

  auto tp = C::system_clock::now(); // tp is a C::system_clock::time_point
  {
    // Need to reach into namespace date for this streaming operator
    using namespace date;
    S::cout << tp << '\n';
  }
  auto dp = D::floor<D::days>(tp);  // dp is a sys_days, which is a
  // type alias for a C::time_point
  auto ymd = D::year_month_day{dp};
  auto time = D::make_time(C::duration_cast<C::milliseconds>(tp-dp));
  S::cout << "year        = " << ymd.year() << '\n';
  S::cout << "month       = " << ymd.month() << '\n';
  S::cout << "month       = " << unsigned(ymd.month()) << '\n';
  S::cout << "day         = " << ymd.day() << '\n';
  S::cout << "hour        = " << time.hours().count() << "h\n";
  S::cout << "minute      = " << time.minutes().count() << "min\n";
  S::cout << "second      = " << time.seconds().count() << "s\n";
  S::cout << "millisecond = " << time.subseconds().count() << "ms\n";
}
