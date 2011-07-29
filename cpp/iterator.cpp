#include <vector>
#include <iostream>

int main(int argc, char const* argv[])
{
  using namespace std;

  vector<int> c = { 1, 3, 5, 7, 9 };

  for (auto i=c.begin(), end=c.end(); i<end; ++i)
    cout << *i;

  return 0;
}
