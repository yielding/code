#include <cstring>
#include <string>
#include <iostream>
#include <algorithm>

using namespace std;

void test_memmove()
{
  string s = "leech";
  for (size_t i=0; i<s.length(); ++i)
  {
    char c = s[0];
    memmove(&s[0], &s[1], s.length());
    s[s.length()-1] = c;
    cout << s << endl;
  }
}

void test_rotate()
{
  string s = "leech";
  for (size_t i=0; i<s.length(); ++i)
  {
    rotate(s.begin(), s.begin()+1, s.end());
    cout << s << endl;
  }
}

int main()
{
  test_memmove();
  test_rotate();

  return 0;
}

