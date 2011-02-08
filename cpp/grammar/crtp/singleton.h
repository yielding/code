#ifndef SINGLETON_H_WNOO550Q
#define SINGLETON_H_WNOO550Q

template <typename T>
class Singleton
{
public:
  Singleton()
  {
    m_instance = static_cast<T*>(this);
  }

  ~Singleton()
  {
    m_instance = 0;
  }

  static T& instance()     { return *m_instance; }
  static T* instance_ptr() { return  m_instance; }

protected:
  static T* m_instance;
};

template<typename T> T* Singleton<T>::m_instance = 0;

#endif /* end of include guard: SINGLETON_H_WNOO550Q */

#if 0
#include <iostream>

using namespace std;

class S: public Singleton<S>
{
public:
  S()
  {
    cout << "S is created\n";
  }

  void print()
  {
    cout << "S.print() is called\n";
  }
};

int main(int argc, char const* argv[])
{
  S& s = S::instance();

  s.print();

  return 0;
}
#endif
