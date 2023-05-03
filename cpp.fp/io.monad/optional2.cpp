#include <iostream>
#include <functional>

using namespace std;

template<class T>
class optional
{
public:
  optional() : _isValid(false) {} // Nothing
  optional(T x) : _isValid(true) , _v(x) {} // Just

  auto isValid() const { return _isValid; }
  auto val()     const { return _v; }

  template <typename R>
  auto fmap(function<optional<R>(T)>& functor) -> optional<R>
  {
    return isValid()
      ? functor(_v)
      : optional<R> {};
  }

  template <typename R>
  auto map(function<R(T)>& f) -> optional<R>
  {
    function<optional<R>(T)> res = [f](T v) {
      return optional<R>(f(v));
    };

    return fmap(res);
  }

private:
  bool _isValid; // the tag
  T _v;
};

/*
// curried version
template <typename A, typename B>
auto fmap(function<B(A)> f) -> function<optional<B>(optional<A>)>
{
  return [f](optional<A> opt) {
    return opt.isValid()
      ? optional<B> { f(opt.val()) }
      : optional<B> {};
  };
}

// uncurried version
template <typename A, typename B>
auto fmap(function<B(A)> f, optional<A> opt) -> optional<B>
{
  return opt.isValid()
    ? optional<B> { f(opt.val()) }
    : optional<B> { };
}
*/

template <typename R, typename T>
auto fmap(optional<T> opt, function<R(T)> f) -> optional<R>
{
  return opt.isValid()
    ? optional<R> { f(opt.val()) }
    : optional<R> { };
}

auto cube(int i) -> optional<double>
{
  return optional<double>{};
}

int main(int argc, char *argv[])
{
  optional<int> a(2);
  optional<int> b(4);

  function<int(int)> f =
    [](int i) -> int { return i*i; };

  function<optional<double>(int)> cube2 =
    [](int i) { return optional<double>(i*i*i); };

  function<optional<double>(int)> xx = cube;

  auto c = a.map(f).fmap(xx);
  cout << c.isValid() << endl;

  auto d = fmap(b, f);
  cout << d.val() << endl;

  optional<int> none;
  auto e = fmap(none, f);
       e = fmap(e, f);
  cout << e.isValid() << endl;


  return 0;
}
