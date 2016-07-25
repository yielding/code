#ifndef DETAIL_WIN32_DYLIB_H__
#define DETAIL_WIN32_DYLIB_H__

#include <string>
#include <windows.h>

namespace sys { namespace detail { namespace win32 {
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

  typedef HMODULE handle_type;

public:
  dylib()
    : m_last_error("")
    , m_handle(NULL)
  {}

  bool open(std::string const& lib_name)
  {
    m_handle = ::LoadLibraryA(lib_name.c_str());
    bool ok = m_handle != NULL;
    if (ok)
      return true;

    LPVOID lpMsgBuf;
    DWORD dwRet = ::FormatMessageA(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, 
      0, ::GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
      (LPSTR)&lpMsgBuf, 0, 0
    );

    if (dwRet == 0)
      m_last_error = "FormatMessage에서 에러 발생";
    else
      m_last_error = (char*)lpMsgBuf;

    ::LocalFree(lpMsgBuf);

    return false;
  }

  void close()
  {
    ::FreeLibrary(m_handle);
  }

  creator_t* get_creator(char* name)
  {
    creator_t* creator = (creator_t*) ::GetProcAddress(m_handle, name);
    return creator;
  }

  destroyer_t* get_destroyer(char* name)
  {
    destroyer_t* destroyer = (destroyer_t*) ::GetProcAddress(m_handle, name);
    return destroyer;
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
