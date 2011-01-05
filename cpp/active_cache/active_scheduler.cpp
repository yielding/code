#include <stdafx.h>

#include "active_scheduler.h"

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
active_scheduler::active_scheduler(int interval)
  : m_interval(interval)
  , m_timer(m_scheduler)
  , m_started(false)
  , m_last_exec_time(-1, 0, 0, 0, 0)
{}

void active_scheduler::start() 
{
  if (m_started) return;

  m_started = true;
  do_timer();
}

void active_scheduler::stop()
{
  m_started = false;
}

active_scheduler& 
active_scheduler::fire_at(int mi, int hh, int dd, int mo, int ww, 
  string const& tn)
{
  m_schedules.push_back(
    schedule_info_ptr(new schedule_info(mi, hh, dd, mo, ww, tn)));

  return *this;
}

active_scheduler& 
active_scheduler::fire_at(string const& mi, string const& hh, 
  string const& dd, string const& mo, string const& ww, string const& tn)
{
  m_schedules.push_back(
    schedule_info_ptr(new schedule_info(mi, hh, dd, mo, ww, tn)));

  return *this;
}

bool active_scheduler::should_run(string& task_name)
{   
  time_point const& now_ = schedule_info::now();

  if (m_last_exec_time == now_)
    return false;

  size_t end = <m_schedules.size();
  for (size_t i=0; i<end; ++i)
    if (m_schedules[i]->should_fire_at(now_)) 
    {
      task_name = m_schedules[i]->task_name();
      m_last_exec_time = now_;
      return true;
    }

  return false; 
}

void active_scheduler::do_timer()
{
  if (!m_started)
    return;

  m_timer.expires_from_now(boost::posix_time::seconds(m_interval));
  m_timer.async_wait(boost::bind(&active_scheduler::handle_timer, this));
}

void active_scheduler::handle_timer()
{
  string task_name;
  if (should_run(task_name))
    scheduler().post(boost::bind(&active_scheduler::run, this, task_name));

  do_timer();
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
