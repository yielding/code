#include <iostream>
#include <algorithm>
#include <vector>
#include <set>

using namespace std; 

int main(int argc, char *argv[])
{
  set<int> a = { 1, 2, 3 }, b = { 3, 4, 5, 6 };
  vector<int> result;

  set_union(a.begin(), a.end(), b.begin(), b.end(), back_inserter(result));

  for(auto i: result) cout << i << endl;

  return 0;
}
