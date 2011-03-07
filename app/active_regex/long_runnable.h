#ifndef LONG_RUNNABLE_H_DTTWC7K3
#define LONG_RUNNABLE_H_DTTWC7K3

#include <boost/function.hpp>

////////////////////////////////////////////////////////////////////////////////
//
// Notice Arg should be a struct when we need arguments more thant 2
//
////////////////////////////////////////////////////////////////////////////////
template <typename R, typename Arg>
class long_runnable
{
public:
  typedef boost::function1<R, Arg> LRFunc;
  typedef boost::function0<bool> StopFunc;

public:
  template <typename Func>
  void attach_notifier(Func f) { m_notifier = f; }

  template <typename Func>
  void attach_stop_checker(Func f) { m_stop_checker = f; }

  bool should_stop()
  { 
    return m_stop_checker.empty() ? false : m_stop_checker();
  }
  
  R notify(Arg const& arg) 
  {
    return m_notifier.empty() ? R() : m_notifier(arg);
  }

private:
  LRFunc m_notifier;
  StopFunc m_stop_checker;
};

template <typename R>
class long_runnable<R, void>
{
public:
  typedef boost::function0<R> LRFunc;
  typedef boost::function0<bool> StopFunc;

public:
  template <typename Func>
  void attach_notifier(Func f) { m_notifier = f; }

  template <typename Func>
  void attach_stop_checker(Func f) { m_stop_checker = f; }

  bool should_stop()
  { 
    return m_stop_checker.empty() ? false : m_stop_checker();
  }
  
  R notify()
  {
    return m_notifier.empty() ? R() : m_notifier();
  }

private:
  LRFunc m_notifier;
  StopFunc m_stop_checker;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
