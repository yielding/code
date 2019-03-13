#ifndef TIME_STAMP_H_
#define TIME_STAMP_H_

#include <memory>
#include <string>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace sys {

  class TimeStampImpl;

  class TimeStamp
  {
  public:
    TimeStamp();
    TimeStamp(int y, int m, int d, int h, int mm, int s, int ms);
    TimeStamp(TimeStampImpl* impl);
    TimeStamp(time_t t);
   ~TimeStamp();

  public:
    auto to_s() const -> std::string;
    auto year() const -> int;
    auto month() const -> int;
    auto day() const -> int;
    auto hour() const -> int;
    auto minute() const -> int;
    auto second() const -> int;

  public:
    static auto now() -> TimeStamp;

  private:
    std::unique_ptr<TimeStampImpl> m_impl;
  };

}
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
