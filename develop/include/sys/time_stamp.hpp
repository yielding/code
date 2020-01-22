#pragma once

#include <memory>
#include <string>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace sys {

  class TimeStamp
  {
  public:
    TimeStamp();
    TimeStamp(int y, int m, int d, int h, int mm, int s, int ms);
    TimeStamp(time_t t);
    TimeStamp(TimeStamp && rhs);
    TimeStamp(TimeStamp const& rhs);
   ~TimeStamp();
    
    TimeStamp& operator=(TimeStamp const& rhs);

  public:
    auto to_s() const -> std::string;
    
    int year();
    int month();
    int day();
    int hour();
    int minute();
    int second();

  public:
    static auto now() -> TimeStamp;
    static auto min_value() -> TimeStamp;

  private:
    class impl; std::unique_ptr<impl> m_impl;
    impl* impl_()             { return m_impl.get(); }
    const impl* impl_() const { return m_impl.get(); }
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
