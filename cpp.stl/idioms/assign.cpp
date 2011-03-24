#include <algorithm>
#include <iterator>
#include <iostream>
#include <vector>

using namespace std;

void p(vector<int> const& v)
{
  copy (v.begin(), v.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}

int main(int argc, char const* argv[])
{
  int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int size = sizeof a / sizeof a[0];

  vector<int> v1(a, a + size);
  p(v1);

  vector<int> v2;
  v2.assign(v1.begin() + 5, v1.end());
  p(v2);
  cout << v2.size();

  char b[] = { 'a', 'b', 55, 56, 57, 58, 59, 60 };
  size = sizeof b / sizeof b[0];
  string c;
  c.assign(b, b + size);
  cout << c;

  return 0;
}
