#include <iostream>
#include <functional>

using namespace std;

template <typename T>
class memoization
{
public:
  memoization(function<T()> func) 
    : m_func(func)
    , m_subroutine(&force_subroutine)
    , m_recorded_func(T())
  {
  }

  T Fetch()
  {
    return m_subroutine(this);
  }

private:
  T const& (*m_subroutine)(memoization* );
  mutable T m_recorded_func;
  function<T()> m_func;

private:
  static T const& force_subroutine(memoization* d)
  {
    return d->do_recording();
  }

  static T const& fetch_subroutine(memoization* d)
  {
    return d->fetch_recording();
  }

  T const& fetch_recording()
  {
    return m_recorded_func;
  }

  T const& do_recording()
  {
    m_recorded_func = m_func();
    m_subroutine = &fetch_subroutine;

  }
};

int main(int argc, char *argv[])
{
  int a = 10;
  int b = 20;

  int multiplexer = 0;
  
  memoization<int> multiply_impure([&] () 
  { 
    return multiplexer * a * b; 
  });

  for (int i = 0; i < 10; ++i) 
  {
    ++multiplexer;
    cout << "a * b = " << multiply_impure.Fetch() ;
    cout << endl;
  }
  
  return 0;
}

