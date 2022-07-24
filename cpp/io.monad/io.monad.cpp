#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include <functional>

using namespace std;

struct U {};

template<typename T>
class IO
{
public:
  IO (function<T()> f) :_act(f) {}

  T run() { return _act(); }

  template<typename F>
  auto bind(F f) //-> decltype(f(_act()))
  {
    auto act = _act;
    return IO<decltype(f(_act()).run())>(
        [act, f]() {
          T x = act();
          return f(x).run();
        });
  }

  template<typename F>
  auto fmap(F f) //-> decltype(f(_act()))
  {
    auto act = _act;
    return IO<decltype(f(_act()))>(
        [act, f]() {
          T x = act();
          return f(x);
        });
  }

private:
  function<T()> _act;
};


IO<U> putStr(string s)
{
  return IO<U>(
      [s]() { cout << s; return U(); }
  );
}

IO<string> getLine(U)
{
  return IO<string>(
    []() { string s; getline(cin, s); return s; }
  );
}

string upcase(string s)
{
  transform(s.begin(), s.end(), s.begin(), 
      [](unsigned char c) { return std::toupper(c); }
      );

  return s;
}

IO<U> test()
{
  return putStr("Tell me your name!\n")
    .bind(getLine)
    .fmap(upcase)
    .bind([](string s) { return putStr("Hi " + s + "\n"); });
}


template <typename T>
IO<T> pure (T x) 
{
  return IO<T>([x]() { return x; });
}

IO<int> guess(int a, int b)
{
  if (a >= b)
    return pure(a);

  int m = (b + a + 1) / 2;

  return ask(m).bind(
    [=](bool yes) { return yes ? guess(a, m-1) : guess(m, b); }
  );
}

IO<bool> ask(int i)
{
  return putStr("Is it less than ")
    .bind([i](U) { return }
}

int main(int argc, char *argv[])
{
  test().run();
  
  return 0;
}
