#include <vector>
#include <iostream>

using namespace std;

int deduplicate(vector<int>& ints)
{
  if (ints.empty())
    return 0;

  int pos = 0;
  for (int i=1; i<ints.size(); ++i)
  {
    if (ints[pos] != ints[i])
    {
      pos++;
      ints[pos] = ints[i];
    }
  }

  auto len = pos + 1;
  ints.erase(ints.begin() + len, ints.end());

  return len;
}

int main(int argc, char *argv[])
{
  vector<int> ints { 0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4 };

  cout << deduplicate(ints) << endl;

  for (auto i: ints) cout << i << " ";
  cout << endl;

  return 0;
}
