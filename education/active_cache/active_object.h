#pragma once

#include <boost/asio.hpp>
#include <thread>
#include <memory>

namespace sys {
////////////////////////////////////////////////////////////////////////////////
//
// active_object 사용에 있어서의 핵심은 io_service안에서 실행되게 하는것이다. 
// 그래야만 
//   (1) caller, callee가 분리된다.
//   (2) callee내부에서 연속되는 호출에 대해 동기화가 보장된다.
//
//////////////////////////////////////////////////////////////////////////////// 
using namespace std;
using io_service = boost::asio::io_service;

class active_object
{
public:
  active_object(): m_work(m_scheduler)
  {
    auto t = new thread([this]() { m_scheduler.run(); });
    m_executor.reset(t);
  }

  virtual ~active_object()
  {
    m_scheduler.stop();
    m_executor->join();
  };

  active_object(const active_object&) = delete;

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
  auto scheduler() -> io_service& 
  {
    return m_scheduler;
  }

private:
  io_service m_scheduler;

private:
  io_service::work m_work;
  shared_ptr<thread> m_executor;
};

//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
} // end of sys
