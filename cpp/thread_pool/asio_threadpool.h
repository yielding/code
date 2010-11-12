#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>

namespace sys {
//////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////// 
class threadpool: private boost::noncopyable 
{
public:
  threadpool(std::size_t init_size=5)
    : m_init_thread(init_size)
    , m_work(m_scheduler)
  {}

  virtual ~threadpool()
  {
    stop();
  }

  void start()
  {
    for (std::size_t i=0; i<m_init_thread; ++i)
      m_threads.create_thread(
        boost::bind(&boost::asio::io_service::run, &m_scheduler));
  }

  void stop()
  {
    m_scheduler.stop();
    m_threads.interrupt_all();
    m_threads.join_all();
  }

  void pool_size(std::size_t size)
  {
    m_init_thread = size;
  }

  template<typename Handler>
  bool post(Handler h)
  {
    m_scheduler.post(h);
    return true;
  }

  template<typename Handler>
  bool dispatch(Handler h)
  {
    m_scheduler.dispatch(h);
    return true;
  }

  boost::asio::io_service& scheduler()
  {
    return m_scheduler;
  }

  void block()
  {
    m_scheduler.run();
  }

private:
  std::size_t m_init_thread;

private:
  boost::thread_group m_threads;
  boost::asio::io_service m_scheduler;
  boost::asio::io_service::work m_work;
};

//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
}
