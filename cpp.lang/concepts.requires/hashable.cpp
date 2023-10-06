#include <algorithm>
#include <iostream>
#include <concepts>

using namespace std;

template <typename T>
concept Hashable = requires(T a)
{
  { hash<T>{} (a) } -> convertible_to<size_t>;
};

struct meow {};

template <typename T>
void f(T) requires Hashable<T>
{}

int main(int argc, char *argv[])
{
  using std::operator""s;

  f("abc"s);
  f(meow{});

  return 0;
}