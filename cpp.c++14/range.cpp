#include <iostream>
#include <string>
#include <vector>

using namespace std;

template <typename T>
class range_iterator
{
public:
  range_iterator(T init) : m_cur{init} {}

  range_iterator& operator++()
  {
    m_cur += m_step;

    return *this;
  }

  bool operator != (range_iterator<T> const& rhs) const 
  {
    return m_cur != rhs.m_cur;
  }

  T operator*() const 
  {
    return m_cur;
  }

private:
  T m_cur;
  T const m_step = 1;
};

template <typename T>
class range_impl
{
public:
  range_impl(T start, T stop) : m_start(start), m_stop(stop)
  {
  }

  auto begin() const -> range_iterator<T> 
  {
    return range_iterator<T>(m_start);
  }

  auto end() const -> range_iterator<T> 
  {
    return range_iterator<T>(m_stop);
  }

private:
  T const m_start;
  T const m_stop;
};

template <typename T>
auto range(T const start, T const stop) -> range_impl<T> 
{
  return range_impl<T>(start, stop);
}

int main(int argc, char *argv[])
{

  for (auto i : range(1, 10))
    cout << i << endl;
  
  return 0;
}
