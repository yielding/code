#include <iostream>
#include <functional>
#include <print>


template <typename T>
class optional
{
public:
  optional() : _is_valid{false} {}          // Nothing
  optional(T x) : _is_valid{true}, _v{x} {} // Just

  auto isValid() const { return _is_valid; }
  auto val()     const { return _v; }

  template <typename R>
  auto fmap(std::function<optional<R>(T)>& functor) -> optional<R>
  {
    return isValid()
      ? functor(_v)
      : optional<R> {};
  }

  template <typename R>
  auto map(std::function<R(T)>& f) -> optional<R>
  {
    std::function<optional<R>(T)> res = [f](T v) {
      return optional<R>(f(v));
    };

    return fmap(res);
  }

private:
  bool _is_valid; // the tag
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
auto fmap(optional<T> opt, std::function<R(T)> f) -> optional<R>
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

  std::function<int(int)> f = [](int i) -> int { return i*i; };

  std::function<optional<double>(int)> cube2 = [](int i) { return optional<double>(i*i*i); };
  std::function<optional<double>(int)> xx = cube;

  auto c = a.map(f).fmap(xx);
  std::print("{}\n", c.isValid());

  auto d = fmap(b, f);
  std::print("{}\n", d.val());

  optional<int> none;
  auto e = fmap(none, f);
  e = fmap(e, f);
  std::print("{}\n", e.isValid());

  return 0;
}
