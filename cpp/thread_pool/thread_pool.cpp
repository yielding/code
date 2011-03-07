#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/thread.hpp>

#include <iostream>
#include <boost/thread/mutex.hpp>
#include <boost/format.hpp>

#include <boost/timer.hpp>

using namespace std;
using namespace boost;

mutex io_mutex;

namespace sys {
//////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////// 
class thread_pool: private boost::noncopyable 
{
public:
  thread_pool(std::size_t init_size=5, int min_size=5, int max_size=5)
    : m_init_thread(init_size)
    //, m_timer(m_scheduler, boost::posix_time::seconds(1)) 
    , m_cur_thread(0)
    , m_wait_thread(0)
    , m_min_thread(min_size)
    , m_max_thread(max_size)
    , m_queue_length(0)
    , m_work(m_scheduler)
  {
    using namespace boost;

    for (std::size_t i=0; i<m_init_thread; ++i)
    {
      //m_threads.create_thread(bind(&sys::thread_pool::thread_method, this));
      m_threads.create_thread(bind(&asio::io_service::run, &m_scheduler));
      ++m_cur_thread;
    }

//    m_timer.async_wait(bind(&sys::thread_pool::balancer, this));
  }

  virtual ~thread_pool()
  {
    m_scheduler.stop();
    m_threads.interrupt_all();
    m_threads.join_all();
  }

  std::size_t queue_length()
  {
    mutex::scoped_lock l(m_atomic);
    return m_queue_length;
  }

  template<typename Handler>
  bool post(Handler h)
  {
//    if (m_cur_thread < m_max_thread)
//    {
//      mutex::scoped_lock l(m_atomic); 
//      m_threads.create_thread(bind(&sys::thread_pool::thread_method, this));
//      m_cur_thread++;
//    }

    { 
      mutex::scoped_lock 
      l(m_atomic); 
      m_scheduler.post(h);
      m_queue_length++; 
    }

    return true;
  }

  /*
  template<typename Handler>
  bool dispatch(Handler h)
  {
    if (m_wait_thread < 3 && m_cur_thread < m_max_thread)
    {
      mutex::scoped_lock l(m_atomic); 
      m_threads.create_thread(bind(&sys::thread_pool::thread_method, this));
      m_cur_thread++;
    }

    m_scheduler.dispatch(h);
    { mutex::scoped_lock l(m_atomic); m_queue_length++; }

    return true;
  }
  */

  void statistics()
  {
    cout << str(format("max:%2d min:%2d wait:%2d cur:%2d que:%2d\n") 
        % m_max_thread % m_min_thread % m_wait_thread % m_cur_thread % m_queue_length);
  }

private:
  /*
  void thread_method()
  {
    bool loop = true;
    while (loop)
    {
      { mutex::scoped_lock l(m_atomic); m_wait_thread++; }
      int hc = m_scheduler.poll_one();
      { mutex::scoped_lock l(m_atomic); m_wait_thread--; }

      if (hc > 0)
      {
        mutex::scoped_lock l(m_atomic); 
        m_queue_length = m_queue_length - hc;
      }
      else
      { 
        if (m_cur_thread > m_min_thread)
        {
          mutex::scoped_lock l(io_mutex);
          cout << "delete useless thread: " <<  this_thread::get_id() << "\n";
          loop = false;
        }
        else
        {
//          mutex::scoped_lock l(io_mutex);
//          cout << "wait ....\n";
//          this_thread::sleep(posix_time::milliseconds(50));
        }
      }
    }

    { mutex::scoped_lock l(m_atomic); m_cur_thread--; }
  }
  */

//  void balancer()
//  {
//    using namespace boost;
//    m_timer.expires_at(m_timer.expires_at() + posix_time::seconds(1));
//    m_timer.async_wait(bind(&sys::thread_pool::balancer, this));
//    statistics();
//  }

private:
  boost::asio::io_service m_scheduler;
  boost::asio::io_service::work m_work;
//  boost::asio::deadline_timer m_timer;
  boost::thread_group m_threads;

  boost::mutex m_atomic;

private:
  std::size_t m_init_thread;
  std::size_t m_cur_thread;
  std::size_t m_wait_thread;
  std::size_t m_min_thread;
  std::size_t m_max_thread;
  std::size_t m_queue_length;
};

//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
}

sys::thread_pool pool(5);

void hello(int value)
{
  { 
    mutex::scoped_lock l(io_mutex);
    cout << "thead id: " << this_thread::get_id() << " "; 
    cout << "hello value : " << value << " ";
    pool.statistics();
    // this_thread::sleep(posix_time::milliseconds(100));
    
    cout.flush();
  }
}

int main(int argc, char const* argv[])
{
  // cout << "thread pool created\n";

  for (int i=0; i<100; i++) pool.post(bind(hello, i));
  this_thread::sleep(posix_time::milliseconds(100));

  for (int i=0; i<100; i++) pool.post(bind(hello, i));
  this_thread::sleep(posix_time::milliseconds(100));

  cout << "ok\n";

  sleep(10000);

  return 0;
}
