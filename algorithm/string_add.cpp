#include <iostream>
#include <sstream>
#include <algorithm>

using namespace std;

string add(string const& a, string const& b) 
{
  stringstream res;

  auto acur = a.rbegin();
  auto bcur = b.rbegin();
  auto carry = 0;

  while (acur != a.rend() || bcur != b.rend()) 
  {
    auto f = (acur == a.rend()) ? 0 : (*acur++ - '0');
    auto s = (bcur == b.rend()) ? 0 : (*bcur++ - '0');

    res <<  (f + s + carry) % 10;
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
  string a = "123";
  string b = "12345";
  string c = add(a, b);

  cout << c;
  
  return 0;
}
