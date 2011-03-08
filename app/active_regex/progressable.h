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
  typedef boost::function<void (int64_t)> ProgressF;

  template <typename Func>
  void progress_notifier(Func f) { m_progress_notifier = f; }

  void notify_progress(int64_t position)
  {
    if (!m_progress_notifier.empty()) 
      m_progress_notifier(position);
  }

private:
  ProgressF m_progress_notifier;
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
  src.attach_progress_notifier(boost::bind(&ProgressManager::progress, &mgr, ::_1));
  //...
  
  return 0;
}

#endif

