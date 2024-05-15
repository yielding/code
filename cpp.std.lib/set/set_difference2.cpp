#include <algorithm>
#include <vector>
#include <string>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  using Strs = vector<string>;

  Strs s3, s2 = { "leech1" },
           s1 = { "leech1", "leech2", "leech3" };

  set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(), 
      back_inserter(s3));

  for (auto i : s3) cout << i << " ";

  cout << endl;
}
