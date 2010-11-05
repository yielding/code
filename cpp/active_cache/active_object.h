#ifndef ACTIVE_OBJECT_H__
#define ACTIVE_OBJECT_H__

#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/thread.hpp>

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

  // scheduler라는 이름이 자체로 괜찮네.
  // dispatcher(), io_service() 이런것도..
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
#endif
