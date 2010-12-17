#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

using namespace std;

int main()
{
  int sequence[] = {1, 2, 3, 4, 5, 5, 7, 8, 9, 10};
  //
  // Set up a vector.
  //
  vector<int> v(sequence+0,  sequence+10);

  int i = count(v.begin(), v.end(), 5);	// Count fives 
  int j = count(v.begin(), v.end(), 6);	// Count sixes 
  // 
  // Count all less than 8.
  //
  int k = count_if(v.begin(), v.end(), bind2nd(less<int>(), 8));

  cout << i << " " << j << " " << k << endl;

  return 0;
}
