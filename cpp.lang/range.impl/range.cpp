#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename T>
class range_iterator
{
public:
  range_iterator(T init) : cur_{init} {}

  auto operator++() -> range_iterator& 
  {
    cur_ += step_;
    return *this;
  }

  auto operator !=(const range_iterator<T>& rhs) const -> bool
  {
    return cur_ != rhs.cur_;
  }

  T operator*() const 
  {
    return cur_;
  }

private:
  T cur_;
  const T step_ = 1;
};

template <typename T>
class range_impl
{
public:
  range_impl(T start, T stop) : start_{start}, stop_{stop}
  {}

  auto begin() const -> range_iterator<T>
  {
    return range_iterator<T> { start_ };
  }

  auto end() const -> range_iterator<T>
  {
    return range_iterator<T> {stop_};
  }

private:
  const T start_;
  const T stop_;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename T>
auto range(const T start, const T stop) -> range_impl<T>
{
  return range_impl<T> {start, stop};
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  for (auto i: range(10, 15))
    cout << i << endl;

  return 0;
}
