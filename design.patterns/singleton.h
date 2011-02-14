#ifndef SINGLETON_H_WNOO550Q
#define SINGLETON_H_WNOO550Q

#include <boost/noncopyable.hpp>

namespace utility { namespace pattern {
//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
template <typename T>
class Singleton: private boost::noncopyable
{
public:
  static T& instance()     { if (m_p == 0) m_p = new T; return *m_p; }
  static T* instance_ptr() { if (m_p == 0) m_p = new T; return  m_p; }
  static void kill()       { if (m_p != 0) delete m_p; m_p = 0;      }
  static T& ref()          { return instance();      }
  static T* ptr()          { return instance_ptr();  }

protected:
  static T* m_p;
};

template<typename T> T* Singleton<T>::m_p = 0;

} // end of pattern
} // end of utility

//////////////////////////////////////////////////////////////////////////////
//
// 
//
//////////////////////////////////////////////////////////////////////////////
#endif /* end of include guard: SINGLETON_H_WNOO550Q */

#if 0
#include <iostream>

using namespace std;

class S: public utility::pattern::Singleton<S>
{
public:
  S()
  {
    a = 10;
    cout << "S is created with: " << a << endl;
  }

  void print()
  {
    cout << "S.print() is called\n";
  }

  int a;
};

int main(int argc, char const* argv[])
{
  S& s = S::ref();

  s.print();

  return 0;
}
#endif
