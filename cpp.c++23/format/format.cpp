#include <format>
#include <iostream>
#include <string>
#include <string_view>

using namespace std;

template <typename... Args>
auto dyna_print(string_view rt_fmt_str, Args&&... args) -> string 
{
  return vformat(rt_fmt_str, make_format_args(args...));
}

int main() 
{
  cout << format("Hello {}!\n", 1);

  string fmt;
  for (int i{}; i != 3; ++i) 
  {
    fmt += "{} "; // constructs the formatting string
    cout << fmt << " : ";
    cout << dyna_print(fmt, "alpha", 'Z', 3.14, "unused");
    cout << '\n';
  }
}