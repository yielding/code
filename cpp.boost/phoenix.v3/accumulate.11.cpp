#include <iostream>
#include <vector>
#include <numeric>

using namespace std;

int main(int argc, char const* argv[])
{
  vector<string> v = { "leech", "kamin", "gunhee" };

  auto add = [=](int memo, string a) { return memo + a.length(); };
  auto res = accumulate(v.begin(), v.end(), 0, add);

  cout << res << endl;

  return 0;
}
