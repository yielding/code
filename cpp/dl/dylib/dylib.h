#ifndef CORE_DYLIB_H__
#define CORE_DYLIB_H__

#ifdef POSIX
#include "detail/posix_dylib.h"
#define DYLIB_BACKEND sys::detail::posix::dylib
#else
#include "detail/win32_dylib.h"
#define DYLIB_BACKEND sys::detail::win32::dylib
#endif

namespace sys {
////////////////////////////////////////////////////////////////////////////////
//
// template 
//
////////////////////////////////////////////////////////////////////////////////
template <typename T,
          typename P,
          template <typename, typename> class basic_dylib = DYLIB_BACKEND>
class dylib
{
public:
  typedef basic_dylib<T, P> impl_type;
  typedef typename impl_type::creator_t   creator_t;
  typedef typename impl_type::destroyer_t destroyer_t;

public:
  dylib()
    : m_is_open(false)
  {
  }

  bool open(std::string const& lib_name)
  {
    if (m_is_open)
      return true;

    m_is_open = m_impl.open(lib_name);

    return m_is_open;
  }

  void close()
  {
    m_impl.close();
  }

  creator_t* get_creator(char* name="create")
  {
    return m_impl.get_creator(name);
  }

  destroyer_t* get_destroyer(char* name="destroy")
  {
    return m_impl.get_destroyer(name);
  }

  std::string last_error()
  {
    return m_impl.last_error();
  }

private:
  bool m_is_open;
  impl_type m_impl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // end of sys

#endif
