#ifndef ACTIVE_OBJECT_H__
#define ACTIVE_OBJECT_H__

#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/thread.hpp>

namespace sys {
////////////////////////////////////////////////////////////////////////////////
//
// active_object 사용에 있어서의 핵심은 io_service안에서 실행되게 하는것이다. 
// 그래야만 
//   (1) caller, callee가 분리된다.
//   (2) callee내부에서 연속되는 호출에 대해 동기화가 보장된다.
//
//////////////////////////////////////////////////////////////////////////////// 
class active_object: private boost::noncopyable 
{
public:
  active_object(): m_work(m_scheduler)
  {
    m_executor.reset(
      new boost::thread(boost::bind(&boost::asio::io_service::run, &m_scheduler)));
  }

  virtual ~active_object()
  {
    m_scheduler.stop();
    m_executor->join();
  };

  template<typename Handler>
  void post(Handler h)
  {
    m_scheduler.post(h);
  }
  
  // call_stack안에 입력으로 넘어온 handler가 있으면
  // 그넘을 실행하고 아니면 post한다.
  template<typename Handler>
  void dispatch(Handler h)
  {
    m_scheduler.dispatch(h);
  }

  // deprecated
  boost::asio::io_service& scheduler()
  {
    return m_scheduler;
  }

protected:
  boost::asio::io_service m_scheduler;

private:
  boost::asio::io_service::work m_work;
  boost::shared_ptr<boost::thread> m_executor;
};

//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
} // end of sys
#endif
