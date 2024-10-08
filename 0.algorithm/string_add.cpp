#include <iostream>
#include <sstream>
#include <algorithm>

using namespace std;

auto add(string const& a, string const& b) -> string
{
  stringstream res;

  auto acur = a.rbegin();
  auto bcur = b.rbegin();
  auto carry = 0;

  while (acur != a.rend() || bcur != b.rend()) 
  {
    auto f = (acur == a.rend()) ? 0 : (*acur++ - '0');
    auto s = (bcur == b.rend()) ? 0 : (*bcur++ - '0');

    res  << (f + s + carry) % 10;
    carry = (f + s + carry) / 10;
  }

  if (carry == 1) 
    res << 1;

  auto result = res.str();
  reverse(result.begin(), result.end());

  return result;
};

int main(int argc, char *argv[])
{
  auto a = "123"s;
  auto b = "12345"s;
  auto c = add(a, b);

  cout << c;

  return 0;
}