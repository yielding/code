#include "sys/time_stamp.hpp"
#include "date.hpp"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

using namespace std;
using namespace std::chrono;

namespace {

  auto split_time(system_clock::time_point const & tp)
    -> tuple<int, unsigned, unsigned, int, int, int, int>
  {
    auto dp   = floor<date::days>(tp);
    auto ymd  = date::year_month_day(dp);
    auto time = date::make_time(duration_cast<milliseconds>(tp - dp));

    auto year  = int(ymd.year());
    auto month = unsigned(ymd.month());
    auto day   = unsigned(ymd.day());
    auto hour  = int(time.hours().count());
    auto min_  = int(time.minutes().count());
    auto sec   = int(time.seconds().count());
    auto ms    = int(time.subseconds().count());

    return make_tuple(year, month, day, hour, min_, sec, ms);
  }

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace sys {

  class TimeStamp::impl
  {
  private:
    friend class TimeStamp;

    impl() : impl(0, 0, 0, 0, 0, 0, 0)
    {}

    impl(int y, int m, int d, int h, int mm, int s, int ms)
    {
      m_tp = date::sys_days(date::year{y}/m/d)
             + hours{h} + minutes{mm} + seconds{s} + microseconds{ms};

      auto res = split_time(m_tp);
      m_year  = get<0>(res); m_month = get<1>(res);
      m_day   = get<2>(res); m_hour  = get<3>(res);
      m_min   = get<4>(res); m_sec   = get<5>(res);
      m_ms    = get<6>(res);
    }

    explicit impl(system_clock::time_point const&& tp)
      : m_tp(tp)
    {
      auto res = split_time(m_tp);
      m_year  = get<0>(res); m_month = get<1>(res);
      m_day   = get<2>(res); m_hour  = get<3>(res);
      m_min   = get<4>(res); m_sec   = get<5>(res);
      m_ms    = get<6>(res);
    }

    explicit impl(time_t t)
      : impl(system_clock::from_time_t(t))
    {}

    system_clock::time_point m_tp;
    int m_year, m_month, m_day;
    int m_hour, m_min, m_sec, m_ms;
  };

  //////////////////////////////////////////////////////////////////////////////////

  auto TimeStamp::now() -> TimeStamp
  {
    auto r = split_time(system_clock::now());

    return TimeStamp(get<0>(r), get<1>(r), get<2>(r), get<3>(r),
      get<4>(r), get<5>(r), get<6>(r));
  }

  TimeStamp::TimeStamp()
    : TimeStamp(0, 0, 0, 0, 0, 0, 0)
  {}

  TimeStamp::TimeStamp(int y, int m, int d, int h, int mm, int s, int ms)
    : m_impl(new impl(y, m, d, h, mm, s, ms))
  {}

  TimeStamp::TimeStamp(time_t t)
    : m_impl(new impl(t))
  {}

  TimeStamp::TimeStamp(TimeStamp && rhs)
  {
    m_impl.swap(rhs.m_impl);
  }

  TimeStamp::TimeStamp(TimeStamp const& rhs)
  {
    if (this != &rhs)
      *m_impl = *rhs.m_impl;
  }

  TimeStamp& TimeStamp::operator=(const TimeStamp& rhs)
  {
    if (this != &rhs)
      *m_impl = *rhs.m_impl;

    return *this;
  }

  TimeStamp::~TimeStamp() = default;

  auto TimeStamp::to_s() const -> string
  {
    using namespace date;
    stringstream s; s << impl_()->m_tp;

    return s.str();
  }

  auto TimeStamp::min_value() -> TimeStamp
  {
    return TimeStamp(1970, 1, 1, 0, 0, 0, 0);
  }

  int TimeStamp::year()
  {
    return impl_()->m_year;
  }

  int TimeStamp::month()
  {
    return impl_()->m_month;
  }

  int TimeStamp::day()
  {
    return impl_()->m_day;
  }

  int TimeStamp::hour()
  {
    return impl_()->m_hour;
  }

  int TimeStamp::minute()
  {
    return impl_()->m_min;
  }

  int TimeStamp::second()
  {
    return impl_()->m_sec;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
