#include <iostream>

using namespace std;

template<class F1, class... Fs>
struct overload : F1, overload<Fs...>
{
  using F1::operator();
  using overload<Fs...>::operator();
  overload(F1 f1, Fs... fs) : F1(f1), overload<Fs...>(fs...) {}
};

template<class F1>
struct overload<F1> : F1
{
  using F1::operator();
  overload(F1 f1) : F1(f1) {}
};

template<class... Fs>
auto make_overload(Fs... fs) {
  return overload<Fs...>(fs...);
}

int main(int argc, char *argv[])
{
  auto f = make_overload(
    [](int i)    { cout << i << endl; },
    [](char c)   { cout << c << endl; },
    [](double d) { cout << d << endl; });

  f(10); // int 
  f('a'); // int 
  f(9.99); // double

  return 0;
}
