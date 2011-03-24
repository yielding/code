#ifndef ASYNC_NOTIFIABLE_H_EGIREG1H
#define ASYNC_NOTIFIABLE_H_EGIREG1H

#include <boost/function.hpp>

////////////////////////////////////////////////////////////////////////////////
//
// Notice Arg should be a struct when we need arguments more thant 2
//
////////////////////////////////////////////////////////////////////////////////
template <typename R, typename Arg>
class async_notifiable
{
public:
  typedef boost::function1<R, Arg> ResultF;

public:
  template <typename Func>
  void result_notifier(Func f)
  { 
    m_result_notifier = f;
  }

  R notify_result(Arg const& arg) 
  {
    return m_result_notifier.empty() ? R() : m_result_notifier(arg);
  }

private:
  ResultF m_result_notifier;
};

template <typename R>
class async_notifiable<R, void>
{
public:
  typedef boost::function0<R> ResultF;

public:
  template <typename Func>
  void result_notifier(Func f) { m_result_notifier = f; }

  R notify_result()
  {
    return m_result_notifier.empty() ? R() : m_result_notifier();
  }

private:
  ResultF m_result_notifier;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
