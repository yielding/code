#include <sstream>
#include <iostream>
#include <cassert>

using namespace std;

namespace str
{
  template <typename T> 
  wstring to_str(T const& t)
  {
    wostringstream o;
    o << t;
    return o.str();
  }
}

int main(int argc, char const* argv[])
{
  wstring res = str::to_str(10);
  assert (res == L"10");

  return 0;
}
