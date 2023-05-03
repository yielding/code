#ifndef DETAIL_POSIX_DYLIB_H__
#define DETAIL_POSIX_DYLIB_H__

#include <dlfcn.h>
#include <string>

namespace sys { namespace detail { namespace posix {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename T, typename P>
class dylib
{
public:
  typedef T* creator_t(P*);
  typedef void destroyer_t(T*);

  typedef void* handle_type;

public:
  dylib()
    : m_last_error("")
    , m_handle(NULL)
  {}

  bool open(std::string const& lib_name)
  {
    m_lib_name = lib_name;

    m_handle = dlopen(lib_name.c_str(), RTLD_LAZY);

    return m_handle != NULL;
  }

  void close()
  {
    dlclose(m_handle);
  }

  creator_t* get_creator(char const* name)
  {
    creator_t* c = (creator_t*)dlsym(m_handle, name);
    char const* err = dlerror();
    if (err != NULL) 
    {
      m_last_error = err;
      return NULL;
    }

    return c;
  }

  destroyer_t* get_destroyer(char const* name)
  {
    destroyer_t* d = (destroyer_t*) dlsym(m_handle, name);
    char const* err = dlerror();
    if (err != NULL)
      m_last_error = err;

    return m_last_error.empty() ? d : NULL;
  }

  std::string last_error()
  {
    return m_last_error;
  }

private:
  std::string m_lib_name;
  std::string m_last_error;
  handle_type m_handle;
};

} // end of sys
} // end of detail
} // end of posix

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
