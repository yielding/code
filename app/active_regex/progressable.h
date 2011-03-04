#ifndef PROGRESSABLE_H_D4XAGKQP
#define PROGRESSABLE_H_D4XAGKQP

#include <boost/function.hpp>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class progressable
{
public:
  typedef boost::function<void (int64_t)> F;

  template <typename Func>
  void set_notifier(Func f) { m_notify = f; }

  void notify(int64_t position)
  {
    if (!m_notify.empty()) 
      m_notify(position);
  }

protected:
  F m_notify;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif

#if 0

#include "progressable.h"
#include <boost/iostreams/concepts.hpp>

class FileBaseSource: public io::source, public progressable
{
public:
  FileBaseSource(FileBase* fb, int offset = 0)
    : m_fb(fb) , m_pos(offset) {}

  std::streamsize read(char_type* s, std::streamsize n)
  {
    //...
    notify(m_pos);
    //...
  }
};

class ProgressManager;
{
public:
  void progress(int64_t pos)
  {
    ...;
  }
};

int main(int argc, char const* argv[])
{
  //...
  FileBaseSource src(spBase.get(), start_pos);

  ProgressManager mgr;
  src.set_notifier(boost::bind(&ProgressManager::progress, &mgr, ::_1));
  //...
  
  return 0;
}

#endif

