#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  typedef vector<int>::iterator iterator;
  int d1[10] = {0,1,2,2,3,4,2,2,6,7}; 
  //
  // Set up a vector.
  //
  vector<int> v1(d1+0, d1+10);
  //
  // Try find.
  //
  iterator it1 = find(v1.begin(), v1.end(), 3);
  //
  // Try find_if.
  //
  iterator it2 = find_if(v1.begin(), v1.end(), bind1st(equal_to<int>(), 3));
  //
  // Try both adjacent_find variants.
  //
  iterator it3 = adjacent_find(v1.begin(), v1.end());
  iterator it4 = adjacent_find(v1.begin(), v1.end(), equal_to<int>());
  //
  // Output results.
  //
  cout << *it1 << " " << *it2 << " " << *it3 << " " << *it4 << endl;

  return 0;
}
