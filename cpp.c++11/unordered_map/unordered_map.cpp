#include <unordered_map>
#include <iostream>
#include <cassert>

using namespace std;

int main(int argc, char const* argv[])
{
  typedef unordered_map<string, int> map;

  map x;

  x["one"] = 1;
  x["two"] = 2;
  x["three"] = 3;

  assert(x.at("one") == 1);
  assert(x.at("two") == 2);
  assert(x.find("missing") == x.end());

  for (auto& v: x) cout << v.first << " : " << v.second << endl;

  return 0;
}
