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
  auto fmap(function<R(T)>& f) -> optional<R>
  {
    return isValid()
      ? optional<R> { f(_v) }
      : optional<R> {};
  }

private:
  bool _isValid; // the tag
  T _v;
};

template <typename A, typename B>
auto fmap(function<B(A)> f) -> function<optional<B>(optional<A>)>
{
  return [f](optional<A> opt) {
    return opt.isValid()
      ? optional<B> { f(opt.val()) }
      : optional<B> {};
  };
}

template <typename A, typename B>
auto fmap(function<B(A)> f, optional<A> opt) -> optional<B>
{
  return opt.isValid()
    ? optional<B> { f(opt.val()) }
    : optional<B> { };
}

int main(int argc, char *argv[])
{
  optional<int> a(2);
  optional<int> b(4);

  function<int(int)> f =
    [](int i) -> int { return i*i; };

  auto b = a.fmap(f);

  cout << b.val();

  return 0;
}
