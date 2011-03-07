#ifndef STOPPABLE_H_IJITEO1U
#define STOPPABLE_H_IJITEO1U

#include <boost/function.hpp>

////////////////////////////////////////////////////////////////////////////////
//
// Notice Arg should be a struct when we need arguments more thant 2
//
////////////////////////////////////////////////////////////////////////////////
template <typename Arg>
class stoppable
{
public:
  typedef boost::function1<bool, Arg> F;

public:
  template <typename Func>
  void attach(Func f) { m_notify = f; }

  bool should_stop(Arg const& arg) 
  { 
    return m_notify.empty() ? false : m_notify(arg);
  }

private:
  F m_notify;
};

template <>
class stoppable<void>
{
public:
  typedef boost::function0<bool> F;

public:
  template <typename Func>
  void attach(Func f) { m_notify = f; }

  bool should_stop()
  { 
    return m_notify.empty() ? false : m_notify();
  }

private:
  F m_notify;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
