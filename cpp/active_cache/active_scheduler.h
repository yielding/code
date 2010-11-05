#ifndef _ACTIVE_SCHEDULER_H__
#define _ACTIVE_SCHEDULER_H__

#include "active_object.h"
#include "schedule_info.h"

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class active_scheduler: public active_object
{
public:
  active_scheduler(int interval=2);

  virtual ~active_scheduler() {}

  void start();
  void stop();

  active_scheduler& fire_at(int mi, int hh, int dd, int mo, int ww, 
    string const& task_name);
  active_scheduler& fire_at(string const& mi, string const& hh, 
    string const& dd, string const& mo, string const& ww, string const& tn);

protected:
  virtual void run(string const& task_name) = 0;
  virtual bool should_run(string& task_name);

protected:
  void do_timer();
  void handle_timer();

private:
  int  m_interval;
  bool m_started;
  time_point m_last_exec_time;
  schedule_info_list m_schedules; 
  boost::asio::deadline_timer m_timer;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
#endif
