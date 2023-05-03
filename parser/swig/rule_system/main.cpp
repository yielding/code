#ifdef WIN32
#pragma warning(disable: 4311)
#pragma warning(disable: 4312)
#endif

#include <asio.hpp>
#include <boost/bind.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "rubyeval.h"

extern "C" void  Init_OpenEye();
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace boost;

class Task
{
public:
  Task(asio::io_service& io, int interval=1)
    : m_timer(io, posix_time::seconds(interval)),
      m_interval(interval)
  {
    m_timer.async_wait(bind(&Task::exec, this));
  }

  void exec()
  {
    if (checkup_time())
    {
      fire();
      return;
    }

    RubyEval::instance()->run_file("embed.rb");

    fire();
  }

  void fire()
  {
    m_timer.expires_at(m_timer.expires_at() + posix_time::seconds(m_interval));
    m_timer.async_wait(bind(&Task::exec, this));
  }

  bool checkup_time() 
  { 
    return false;
  }

private:
  int m_interval;

  asio::deadline_timer m_timer;
};

int main()
{
  RubyEval& ruby = *RubyEval::instance();
  Init_OpenEye();

  asio::io_service io;
  Task t1(io, 2);
  io.run();

  ruby.delete_instance();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
