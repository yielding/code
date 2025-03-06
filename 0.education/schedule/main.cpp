#include "stdafx.h"

#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <iostream>

using namespace std;
using namespace boost;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Task
{
public:
  Task(boost::asio::io_context& io, int interval=1)
    : m_timer(io), m_interval(interval)
  {
    m_timer.expires_from_now(boost::posix_time::seconds(2));
    m_timer.async_wait(bind(&Task::exec, this));
  }

  void exec()
  {
    if (checkup_time())
    {
      fire();
      return;
    }

    cout << "Load Interpreter" << "\n";
    cout << "do job " << m_interval << "\n";
    cout << "UnLoad Interpreter" << "\n";

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

////////////////////////////////////////////////////////////////////////////////
//
// setting은 일단 디비 기반으로하고 나중에 구현한다.
// 자료구조
//
// rule file name or buffer
//
// cleanUp
//  cancel Io
//
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main (int argc, char const* argv[])
{
  asio::io_context io;
  Task t1(io, 3);
  Task t2(io, 2);

  boost::thread tt1(bind(&asio::io_context::run, &io));

  while(1)
  {
    cout << "sleeping\n";
    sleep(3);
  }

  tt1.join();

  cout << "thread joined";

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
