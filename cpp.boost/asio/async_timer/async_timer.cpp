#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>
#include <boost/ref.hpp>

#include <iostream>

using namespace std;
using namespace boost;

#if 0
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTMLParser
{
public:
  HTMLParser() : _res(false)
  {}

  void operator()() { _res = parse0(); }

  bool parse0() { sleep(2); return true; }

  bool parse1() { while(true) {}; return true; }

  bool result() { return _res; }

private:
  bool _res;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  HTMLParser parser;

  try
  {
    asio::io_context io_context;

    asio::deadline_timer timer(io_context);

    cout << "before expire\n";
    timer.expires_from_now(posix_time::seconds(5));
    cout << "after expire\n";

    thread t(ref(parser));
    timer.wait();

    if (parser.result() == false) 
    {
      cout << "kill the thread\n";
      t.interrupt();
    }
    else
    {
      cout << "everything is ok\n";
      t.join();
    }
  }
  catch (std::exception& e)
  {
    std::cout << "Exception: " << e.what() << "\n";
  }

  return 0;
}

#endif 

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTMLParser
{
public:
  HTMLParser() : _res(false), _timer(_io_context)
  {}

  bool parse_within(int sec)
  {
    _try_count = sec;

    do_timer();
    
    boost::thread t(bind(&HTMLParser::parse0, this));

    // REMARK 
    //   below run() is waiting for timer
    //
    _io_context.run();

    if (_res == false)
    {
      cout << "fail to complete the parsing\n";
      t.interrupt();
    }
    else
    {
      cout << "everything is ok\n";
      t.join();
    }

    return _res;
  }

  void parse0() { while(1) { sleep(1); }; }

  void parse1() 
  {
    cout << "run 2 secs\n"; 
    sleep(2);
    _res = true; 
  }

  void do_timer()
  {
    if (_res == true || _try_count <= 0)
      return;

    _try_count--;
    cout << "try_count : " << _try_count << endl;

    _timer.expires_from_now(posix_time::seconds(1));
    _timer.async_wait(bind(&HTMLParser::do_timer, this));
  }

private:
  bool _res;
  int  _try_count;
  asio::io_context _io_context;
  asio::deadline_timer _timer;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  HTMLParser parser;
  cout << (parser.parse_within(10) ? "ok" : "fail");

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
