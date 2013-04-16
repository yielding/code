#include <iostream>
#include <string>
#include <vector>
#include <deque>

using namespace std;

int main(int argc, const char *argv[])
{
  string s = "leech kamIN";
  vector<string> v;
  deque<string> d;
  
  s.shrink_to_fit();
  v.shrink_to_fit();
  d.shrink_to_fit();

  return 0;
}
