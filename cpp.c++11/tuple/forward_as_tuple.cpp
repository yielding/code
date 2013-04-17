#include <iostream>
#include <map>
#include <string>
#include <tuple>

using namespace std;

int main(int argc, const char *argv[])
{
  map<int, string> m;
  m.emplace(forward_as_tuple(10, string(20, 'a')));

  cout << m[10] << endl;

  auto t = forward_as_tuple(11, string(10, 'b'));

  m.emplace(t);
  cout << m[11] << endl;

  return 0;
}
