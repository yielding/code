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

// m_threads.create_thread(bind(&asio::io_service::run, &m_scheduler));

namespace sys {
//////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////// 
class thread_pool: private boost::noncopyable 
{
public:
  thread_pool(std::size_t init_size=5, int min_size=5, int max_size=10)
    : m_init_thread(init_size)
    , m_timer(m_scheduler, boost::posix_time::seconds(1)) 
    , m_cur_thread(0)
    , m_wait_thread(0)
    , m_min_thread(min_size)
    , m_max_thread(max_size)
    , m_queue_length(0)
  {
    using namespace boost;
    m_timer.async_wait(bind(&sys::thread_pool::balancer, this));
    m_queue_length++;

    for (std::size_t i=0; i<m_init_thread; ++i)
    {
      m_threads.create_thread(bind(&sys::thread_pool::thread_method, this));
      ++m_cur_thread;
    }
  }

  virtual ~thread_pool()
  {
    m_scheduler.stop();
    m_threads.interrupt_all();
    m_threads.join_all();
  }

  std::size_t queue_length()
  {
    return m_queue_length;
  }

  template<typename Handler>
  bool post(Handler h)
  {
    //if (m_wait_thread < 3 && m_cur_thread < m_max_thread)
    if (m_cur_thread < m_max_thread)
    {
      mutex::scoped_lock l(m_atomic); 
      m_threads.create_thread(bind(&sys::thread_pool::thread_method, this));
      m_cur_thread++;
    }

    m_scheduler.post(h);
    { mutex::scoped_lock l(m_atomic); m_queue_length++; }

    return true;
  }

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

  void statistics()
  {
    cout << "\nstat: m n w c q :";
    cout << str(format("%2d %2d %2d %2d %2d\n") 
        % m_max_thread % m_min_thread % m_wait_thread % m_cur_thread % m_queue_length);
  }

private:
  void thread_method()
  {
    bool loop = true;
    while (loop)
    {
      { mutex::scoped_lock l(m_atomic); m_wait_thread++; }
      int hc = m_scheduler.run_one();
      { mutex::scoped_lock l(m_atomic); m_wait_thread--; }

      if (hc)
      {
        mutex::scoped_lock l(m_atomic); 
        m_queue_length--;
        if (m_cur_thread > m_min_thread)
        {
          loop = false;
cout << "out...\n";
          continue;
        }
      }
      else
      { 
        mutex::scoped_lock l(io_mutex);
        cout << "queue is empty\n";
        this_thread::sleep(posix_time::milliseconds(10));
      }
    }
    { mutex::scoped_lock l(m_atomic); m_cur_thread--; }
  }

  void balancer()
  {
    using namespace boost;
    m_timer.expires_at(m_timer.expires_at() + posix_time::seconds(1));
    m_timer.async_wait(bind(&sys::thread_pool::balancer, this));
    { mutex::scoped_lock l(m_atomic); m_queue_length++; }
statistics();
  }

private:
  boost::asio::io_service m_scheduler;
  boost::asio::deadline_timer m_timer;
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
  { mutex::scoped_lock l(io_mutex);
    cout << "thead id: " << this_thread::get_id() << " ";
    cout << "hello value : " << value << "\n";
pool.statistics();
    cout.flush();
    this_thread::sleep(posix_time::milliseconds(2000));
  }
}

int main(int argc, char const* argv[])
{
//  timer tt;
//  mutex::scoped_lock l(io_mutex);
//  cout << "time elapsed: " << tt.elapsed() << endl;

cout << "thread pool created\n";

  // pool.statistics();

  for (int i=0; i<100; i++) 
    pool.post(bind(hello, i));

cout << "made 50\n";
pool.statistics();

  sleep(10000);
  asio::io_service io;
  asio::io_service::work blocker(io);
  io.run();

  return 0;
}
