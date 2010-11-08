#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

using namespace std;

int main()
{
  int sequence[10] = {1, 2, 3, 4, 5, 5, 7, 8, 9, 10};
  int i=0, j=0, k=0;
  //
  // Set up a vector.
  //
  vector<int> v(sequence+0,  sequence+10);

  count(v.begin(), v.end(), 5, i);	// Count fives 
  count(v.begin(), v.end(), 6, j);	// Count sixes 
  // 
  // Count all less than 8.
  //
  count_if(v.begin(), v.end(), bind2nd(less<int>(), 8),  k);

  cout << i << " " << j << " " << k << endl;

  return 0;
}
