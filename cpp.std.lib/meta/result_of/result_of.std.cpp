#include <iostream>
#include <type_traits>

using namespace std;

struct S 
{
  double operator()(char, int&);
  float operator()(int);
};

struct C 
{
  double Func(char, int&);
};

int main()
{
  // the result of invoking S with char and int& arguments is double
  result_of<S(char, int&)>::type f = 3.14; // f has type double
  static_assert(is_same<decltype(f), double>::value, "");

  // the result of invoking S with int argument is float
  result_of<S(int)>::type d = 3.14; // f has type float
  static_assert(is_same<decltype(d), float>::value, "");

  // result_of can be used with a pointer to member function as follows
  result_of<decltype(&C::Func)(C, char, int&)>::type g = 3.14;
  static_assert(is_same<decltype(g), double>::value, "");
}
