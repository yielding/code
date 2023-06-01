#ifndef TIME_POINT_H__
#define TIME_POINT_H__

#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>

using namespace std;
//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
struct time_point
{
  time_point(int mi=0, int hh=0, int dd=0, int mo=0, int ww=0)
    : m_min(mi), m_hour(hh), m_day(dd), m_month(mo), m_week(ww)
  {}

  bool operator == (time_point const& rhs)
  {
    return (m_min == rhs.m_min) && (m_hour == rhs.m_hour) &&
           (m_day == rhs.m_day) && (m_month == rhs.m_month) &&
           (m_week == rhs.m_week);
  }
  
  string to_s();

  int m_min;
  int m_hour;
  int m_day;
  int m_month;
  int m_week;
};

class schedule_info
{
public:
  schedule_info(int mi, int hh, int dd, int mo, int ww, string const& task_name);
  schedule_info(time_point const& point, string const& task_name);
  schedule_info(string const& mi, string const& hh, string const& dd, 
    string const& mo, string const& ww, string const& task_name);

  bool should_fire_at(int mi, int hh, int dd, int mo, int ww);
  bool should_fire_at(time_point const& point);
  
  static time_point now();

  string const& task_name() 
  {
    return m_task_name;
  }

protected:
  void ctor(time_point const& tp);

private:
  string const m_task_name;
  vector<int> m_week;
  vector<int> m_month;
  vector<int> m_day;
  vector<int> m_hour;
  vector<int> m_min;
};

typedef boost::shared_ptr<schedule_info> schedule_info_ptr;
typedef vector<schedule_info_ptr> schedule_info_list;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
#endif
