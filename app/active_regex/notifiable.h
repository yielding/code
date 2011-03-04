#ifndef PROGRESSABLE_H_D4XAGKQP
#define PROGRESSABLE_H_D4XAGKQP

#include <boost/function.hpp>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename R, typename Arg>
class notifiable
{
public:
  typedef boost::function1<R, Arg> F;

public:
  template <typename Func>
  void set_notifier(Func f) { m_notify = f; }

  void notify(Arg const& arg) { if (!m_notify.empty()) m_notify(arg); }

protected:
  F m_notify;
};

template <typename R>
class notifiable<R, void>
{
public:
  typedef boost::function0<R> F;

public:
  template <typename Func>
  void set_notifier(Func f) { m_notify = f; }

  void notify() { if (!m_notify.empty()) m_notify(); }

protected:
  F m_notify;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
