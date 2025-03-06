#include "stdafx.h"

#include <boost/asio.hpp>
#include <thread>
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
  Task(asio::io_context& io, int interval=1)
    : _timer(io), _interval(interval)
  {
    _timer.expires_from_now(posix_time::seconds(2));
    _timer.async_wait(bind(&Task::exec, this));
  }

  void exec()
  {
    if (checkup_time())
    {
      fire();
      return;
    }

    cout << "Load Interpreter" << "\n";
    cout << "do job " << _interval << "\n";
    cout << "UnLoad Interpreter" << "\n";

    fire();
  }

  void fire()
  {
    _timer.expires_at(_timer.expires_at() + posix_time::seconds(_interval));
    _timer.async_wait(bind(&Task::exec, this));
  }

  bool checkup_time()
  {
    return false;
  }

private:
  int _interval;

  asio::deadline_timer _timer;
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
int main(int argc, char const* argv[])
{
  asio::io_context io;
  Task t1(io, 3);
  Task t2(io, 2);

  thread tt1(bind(&asio::io_context::run, &io));

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
