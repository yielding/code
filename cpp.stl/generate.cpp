#include <iostream>
#include <vector>
#include <numeric>

using namespace std;

int main()
{
  vector<int> v(10);
  // int i=0; generate(v.begin(), v.end(), [&i](){ return ++i; }); 
  iota(v.begin(), v.end(), 0);
  for (auto i:v) cout << i << " ";
  
  return 0;
}
