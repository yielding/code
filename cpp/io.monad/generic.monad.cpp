#include <iostream>
#include <vector>
#include <iterator>
#include <algorithm>
#include <functional>

using namespace std;

template <template <typename...> class F>
struct Functor 
{
  template <typename A, typename B>
  static auto fmap(function <B(A)>) -> function <F<B>(F<A>)>;
};

template <template <typename...> class F, typename A, typename B>
auto fmap(function <B(A)> f) -> function <F<B>(F<A>)> 
{
  return Functor<F>::fmap(f);
}

template <template <typename...> class F, typename A, typename B>
auto fmap_(function <B(A)> f, F<A> v) -> F<B> 
{
  return Functor<F>::fmap(f)(v);
}

template <template <typename...> class F, typename A, typename B>
auto operator % (function <B(A)> f, F<A> v) -> F<B> 
{
  return Functor<F>::fmap(f)(v);
}

// Monad 
template <template <typename...> class F>
struct Monad 
{
  template <typename A>
  static F<A> return_(A);

  template <typename A, typename B> 
  static F<B> bind(F<A>, function<F<B>(A)>);
};

template <template <typename...> class F, typename A, typename B>
auto bind(F<A> m, function<F<B>(A)> f) -> F<B> 
{
  return Monad<F>::bind(m, f);
}

template <template <typename...> class F, typename A>
auto return_(A a) -> F<A> 
{
  return Monad<F>::return_(a);
}

template <template <typename...> class F, typename A, typename B>
auto operator >= (F<A> m, function<F<B>(A)> f) -> F<B> 
{
  return Monad<F>::bind(m, f);
}

template <template <typename...> class F, typename A, typename B>
auto operator >> (F<A> a, F<B> b) -> F<B> 
{
  function<F<B>(A)> f = [b](A){ return b; };

  return a >= f;
}

// Maybe 
template <typename T>
class Maybe 
{
public:
  Maybe(): _empty(true)
  {};

  explicit Maybe(T value)
    : _empty(false), _value(value)
  {};

  auto from_just() const -> T
  {
    if (is_just()) return _value;

    throw "Cannot get value from Nothing";
  }

  auto is_just() const { return !_empty; }
  auto is_nothing() const { return _empty; }

private:
  bool _empty;
  T _value;
};

template <typename T>
auto operator<<(ostream& s, const Maybe<T> m) -> ostream& 
{
  return m.is_just() 
    ? s << "Just " << m.from_just()
    : s << "Nothing";
}

// Functor Maybe
template <>
struct Functor<Maybe> 
{
  template <typename A, typename B>
  static auto fmap(function <B(A)> f) -> function <Maybe<B>(Maybe<A>)> 
  {
    return [f](Maybe<A> m) -> Maybe<B> {
      return m.is_nothing() ? Maybe<B>() : Maybe<B>(f(m.from_just()));
    };
  };
};

/* Monad Maybe */
template <>
struct Monad<Maybe> 
{
  template <typename A>
  static Maybe<A> return_(A v)
  {
    return Maybe<A>(v);
  }

  template <typename A, typename B>
  static Maybe<B> bind(Maybe<A> m, function<Maybe<B>(A)> f)
  {
    return m.is_nothing() ? Maybe<B>() : f(m.from_just());    
  }
};

// Functor vector
template <>
struct Functor<vector> 
{
  template <typename A, typename B>
  static auto fmap(function<B(A)> f) -> function<vector<B>(vector<A>)> 
  {
    return [f](vector<A> v) {
      vector<B> result;
      transform(v.begin(), v.end(), back_inserter(result), f);
      return result;
    };
  }
};

// Monad vector
template <>
struct Monad<vector> 
{
  template <typename A> 
  static auto return_(A v) -> vector<A> 
  {
    return vector<A>{v};
  }

  template <typename A, typename B>
  static auto bind(vector<A> m, function<vector<B>(A)> f) -> vector<B> 
  {
    vector<B> v;
    for_each(m.begin(), m.end(), [f, &v](A a) {
        vector<B> result = f(a);
        copy(result.begin(), result.end(), back_inserter(v));
    });

    return v;
  }
};

template <typename A, typename B, typename C>
auto compose(function<B(A)> f1, function<C(B)> f2) -> function<C(A)> 
{
  return [f1, f2](A v) -> C {
    return f2(f1(v));
  };
}

int main(int argc, char *argv[])
{
  // Returns the length of a string.
  function<int(string)> length = [](string s) { return s.size(); };

  // Squares an integer arugment.
  function<int(int)> square = [](int i) { return i*i; };

  // Tests function composition.
  cout << "Square of length of \"testing 1234\": " <<
    compose(length, square)("testing 1234") << endl;

  Maybe<string> just("Hello World");
  Maybe<string> nothing;

  cout << "Untouched Maybes:" << endl;
  cout << "\t" << just << endl;
  cout << "\t" << nothing << endl;

  cout << "Maybes fmapped with length:" << endl;
  cout << "\t" << length % just << endl;
  cout << "\t" << length % nothing << endl;

  //
  // A function to test monadic bind on Maybe.
  // Returns Just x if x > 5 otherwise Nothing.
  //
  function<Maybe<int>(int)> fm 
    = [](int v){ return v > 5 ? Maybe<int>(v) : Maybe<int>(); };

  auto good = return_<Maybe>(6);
  auto bad  = return_<Maybe>(4);
  cout << "Before bind:" << endl;
  cout << "\t" << good << endl;
  cout << "\t" << bad << endl;

  cout << "After bind (if x > 5 Just x else Nothing):" << endl;
  //cout << "\t" << bind(good, fm) << endl;
  cout << "\t" << (good >= fm) << endl;
  cout << "\t" << (bad >= fm) << endl;

  function<vector<int>(int)> fv 
    = [](int v){ return vector<int>{v,v*v};};
  cout << "Monadic bind on vector \\v -> [v,v*v]:" << endl;
  vector<int> v{1,2,3,4,5};
  vector<int> vr = v >= fv;
  vector<int> vrr = v >= fv >= fv;
  cout << "\t";
  for (auto& e: v) cout << e << " ";
  cout << endl << "\t";
  copy(vr.begin(), vr.end(), ostream_iterator<int>(cout, " "));
  cout << endl << "\t";
  copy(vrr.begin(), vrr.end(), ostream_iterator<int>(cout, " "));
  cout << endl;

  return 0;
}

