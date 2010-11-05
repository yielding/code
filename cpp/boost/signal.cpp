#include <boost/signal.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/xtime.hpp>

#include <iostream>

using namespace std;

struct ticker
{
  boost::signal<void ()>    signal_tick;
  boost::signal<void (int)> signal_nth_tick;

  void operator() ()
  {
    unsigned int i = 0;
    boost::xtime xt;
    boost::xtime_get(&xt, boost::TIME_UTC);
    xt.sec = 1000;
    while(true)
    {
      boost::thread::sleep(xt);
      signal_tick();
      signal_nth_tick(++i);
    }
  }
};

struct hello_world
{
  void operator() () const
  {
    cout << "Hello, World" << endl;
  }
};

struct star_printer
{
  void operator() (int n) const
  {
    while(n-->0) cout << "*";
    cout << endl;
  }
};

int main()
{
  ticker t;
  t.signal_tick.connect(hello_world());
  t.signal_nth_tick.connect(star_printer());
  t();
}
