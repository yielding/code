#include "sys/time_stamp.h"
#include "date.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace sys {

  using namespace std;
  using namespace std::chrono;

  class TimeStampImpl
  {
  public:
    TimeStampImpl() : TimeStampImpl(0, 0, 0, 0, 0, 0, 0)
    {}

    TimeStampImpl(int y, int m, int d, int h, int mm, int s, int ms)
    {
      m_tp = date::sys_days(date::year{y}/m/d) 
           + hours{h} + minutes{mm} + seconds{s} + microseconds{ms};

      split_time(m_tp);
    }

    TimeStampImpl(system_clock::time_point const& tp)
      : m_tp(tp)
    {
      split_time(m_tp);
    }

    TimeStampImpl(time_t t)
      :m_tp{system_clock::from_time_t(t)}
    {
      split_time(m_tp);
    }

  public:
    auto year()   const -> int { return m_year;  }
    auto month()  const -> int { return m_month; }
    auto day()    const -> int { return m_day;   }
    auto hour()   const -> int { return m_hour;  }
    auto minute() const -> int { return m_min;   }
    auto second() const -> int { return m_sec;   }

    auto to_s() const -> string
    {
      using namespace date;
      stringstream s; s << m_tp;

      return s.str();
    }

  private:
    void split_time(system_clock::time_point& tp)
    {
      auto dp   = floor<date::days>(m_tp);
      auto ymd  = date::year_month_day(dp);
      auto time = date::make_time(duration_cast<milliseconds>(tp - dp));

      m_year  = int(ymd.year());
      m_month = unsigned(ymd.month());
      m_day   = unsigned(ymd.day());
      m_hour  = int(time.hours().count());
      m_min   = int(time.minutes().count());
      m_sec   = int(time.seconds().count());
      m_ms    = int(time.subseconds().count());
    }

  private:
    system_clock::time_point m_tp;
    int m_year, m_month, m_day;
    int m_hour, m_min, m_sec, m_ms;
  };

  //////////////////////////////////////////////////////////////////////////////////
  
  TimeStamp::TimeStamp()
    : m_impl(new TimeStampImpl()) 
  {}

  TimeStamp::TimeStamp(int y, int m, int d, int h, int mm, int s, int ms)
    : m_impl(new TimeStampImpl(y, m, d, h, mm, s, ms)) 
  {}

  TimeStamp::TimeStamp(time_t t)
    : m_impl(new TimeStampImpl(t)) 
  {}

  TimeStamp::TimeStamp(TimeStampImpl* impl)
  {
    m_impl.reset(impl);
  }
      
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
    {
      *m_impl = *rhs.m_impl;
    }
    
    return *this;
  }

  TimeStamp::~TimeStamp() = default;

  auto TimeStamp::to_s() const -> string
  {
    return m_impl->to_s();
  }

  auto TimeStamp::now() -> TimeStamp
  {
    auto impl = new TimeStampImpl(system_clock::now());
    return TimeStamp(impl);
  }

  auto TimeStamp::min_value() -> TimeStamp
  {
    auto impl = new TimeStampImpl(1970, 1, 1, 0, 0, 0, 0);
    return TimeStamp(impl);
  }
      
  auto TimeStamp::year() const -> int
  {
    return m_impl->year();
  }

  auto TimeStamp::month() const -> int
  {
    return m_impl->month();
  }

  auto TimeStamp::day() const -> int
  {
    return m_impl->day();
  }

  auto TimeStamp::hour() const -> int
  {
    return m_impl->hour();
  }

  auto TimeStamp::minute() const -> int
  {
    return m_impl->minute();
  }

  auto TimeStamp::second() const -> int
  {
    return m_impl->second();
  }

}
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
