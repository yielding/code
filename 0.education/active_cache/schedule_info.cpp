#include "stdafx.h"

#include "schedule_info.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <algorithm>

using namespace boost::posix_time;
using namespace boost::gregorian;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
namespace {
void parse_time(string const& ts, int count, vector<int>& time_arr)
{
  time_arr.clear();

  if (boost::trim_copy(ts) == "*")
  {
    for (int i=0; i<count; i++) 
      time_arr.push_back(i);
  }
  else
  {
    vector<string> arr;
    boost::split(arr, ts, boost::is_any_of(","));
    for (size_t i=0; i<arr.size(); ++i) 
      time_arr.push_back(atoi(arr[i].c_str()));
  }
}

bool inside(int time_, vector<int>& arr)
{
  return std::find(arr.begin(), arr.end(), time_) != arr.end();
}
} // end of anonymous namespace

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
string time_point::to_s()
{
  using namespace boost;
  string fmt = "[%d, %d, %d, %d, %d]";
  return str(format(fmt) % m_min % m_hour % m_day % m_month % m_week);
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
schedule_info::schedule_info(int mi, int hh, int dd, int mo, int ww, 
  string const& task_name)
  : m_task_name(task_name)
{
  time_point point(mi, hh, dd, mo, ww);
  ctor(point);
}

schedule_info::schedule_info(time_point const& point, string const& task_name)
  : m_task_name(task_name)
{
  ctor(point);
}

schedule_info::schedule_info(string const& mi, string const& hh, string const& dd, 
  string const& mo, string const& ww, string const& task_name)
  : m_task_name(task_name)
{
  parse_time(mi, 60, m_min);
  parse_time(hh, 24, m_hour);
  parse_time(dd, 31, m_day);
  parse_time(mo, 12, m_month);
  parse_time(ww,  7, m_week);
}

void schedule_info::ctor(time_point const& point)
{
  m_min  .push_back(point.m_min);
  m_hour .push_back(point.m_hour);
  m_day  .push_back(point.m_day);
  m_month.push_back(point.m_month);
  m_week .push_back(point.m_week);
}

bool schedule_info::should_fire_at(time_point const& p)
{
  return should_fire_at(p.m_min, p.m_hour, p.m_day, p.m_month, p.m_week);
}

bool schedule_info::should_fire_at(int mi, int hh, int dd, int mo, int ww)
{
  if (!inside(mi, m_min))   return false;
  if (!inside(hh, m_hour))  return false;
  if (!inside(dd, m_day))   return false;
  if (!inside(mi, m_min))   return false;
  if (!inside(mo, m_month)) return false;
  if (!inside(ww, m_week))  return false;

  return true;
}

time_point schedule_info::now()
{
  string const& st = to_iso_extended_string(second_clock::local_time());
  vector<string> arr;
  boost::split(arr, st, boost::is_any_of("T-:"));

  int mi = atoi(arr[4].c_str());
  int hh = atoi(arr[3].c_str());
  int dd = atoi(arr[2].c_str());
  int mo = atoi(arr[1].c_str());
  int ww = day_clock::local_day().day_of_week().as_number();

  return time_point(mi, hh, dd, mo, ww);
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
