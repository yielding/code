#include <iostream>
#include <functional>
#include <typeinfo>

using namespace std;

template <typename T> struct result;

template <typename T> 
struct success: result<T>
{
  success(T val): result<T>(val) {}

  //auto succeeded() -> bool override { return true; }
};

template <typename T> 
struct fail: result<T> 
{
  fail() {}

  //auto succeeded() -> bool override { return false; }
};

template <typename T>
struct result
{
  result(T val) : m_val {val} { }

  virtual ~result() {}

  //virtual bool succeeded() = 0;

  template <typename R>
  auto fmap(function<R(T)>& f) -> result<R>
  {
    return typeid(*this) == typeid(success<T>)
      ? success<R>(f(m_val))
      : fail<R>();
  }

  template <typename R>
  auto map(function<R(T)>& f) -> result<R>
  {
    return fmap<R>([f](T val) { return success<R>(f(val)); });
    /*
    if (typeid(*this).name() == typeid(success<T>).name())
    {
      return fmap([f](T val) { return success<R>(f(val)); })
    }
    else 
    {
      return fail<R>();
    }
    */
  }

  auto value() const& { return m_val; }

protected:
  T m_val{};
};

auto plus_10(int value) -> result<int>
{
  return success<int>(value + 10);
}

int main(int argc, char *argv[])
{
  function <int(int)> f = [](int a) -> int { return a * 5; };
  plus_10(5).fmap(f);
            //.value();

  return 0;
}
