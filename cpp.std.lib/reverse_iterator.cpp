#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>

using namespace std;

int main(int argc, const char *argv[])
{
  auto alpha = [] (char c) { return c == '1'; };

  auto n = "123leech123"s;
  auto i = find_if(n.begin(),  n.end(), alpha);
  auto j = find_if(n.rbegin(), n.rend(), alpha);

  auto beg = distance(n.begin(), i),
       pos = distance(i, j.base());

  cout << n.substr(beg, pos);

  return 0;
}
