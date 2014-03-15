#include <iostream>
#include <algorithm>

using namespace std;

struct Length
{
  bool operator()(string const& a, string const& b)
  {
    return a.length() < b.length();
  }
};

int int_max()
{
  return max({3, 2, 1});
}

string str_max1()
{
  return max({"Lee1", "leee2", "lee3"}, Length());
}

string str_max2()
{
  return max({"Lee1", "leee2", "lee3"}, 
    [](string const& s1, string const& s2) {
       return s1.size() < s2.size();
    }
  );
}

int main(int argc, const char *argv[])
{
  cout << str_max1() << endl;
  cout << str_max2() << endl;

  return 0;
}
