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
  HTMLParser() : m_res(false)
  {}

  void operator()() { m_res = parse0(); }

  bool parse0() { sleep(2); return true; }

  bool parse1() { while(true) {}; return true; }

  bool result() { return m_res; }

private:
  bool m_res;
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
    asio::io_service io_service;

    asio::deadline_timer timer(io_service);

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
  HTMLParser() : m_res(false), m_timer(m_io_service)
  {}

  bool parse_within(int sec)
  {
    m_try_count = sec;

    do_timer();
    
    thread t(bind(&HTMLParser::parse0, this));

    // REMARK 
    //   below run() is waiting for timer
    //
    m_io_service.run();

    if (m_res == false)
    {
      cout << "fail to complete the parsing\n";
      t.interrupt();
    }
    else
    {
      cout << "everything is ok\n";
      t.join();
    }

    return m_res;
  }

  void parse0() { while(1) { sleep(1); }; }

  void parse1() 
  {
    cout << "run 2 secs\n"; 
    sleep(2);
    m_res = true; 
  }

  void do_timer()
  {
    if (m_res == true || m_try_count <= 0)
      return;

    m_try_count--;
    cout << "m_try_count : " << m_try_count << endl;

    m_timer.expires_from_now(posix_time::seconds(1));
    m_timer.async_wait(bind(&HTMLParser::do_timer, this));
  }

private:
  bool m_res;
  int  m_try_count;
  asio::io_service m_io_service;
  asio::deadline_timer m_timer;
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
