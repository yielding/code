// refer (1)
#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

using namespace std;

int main()
{
  typedef vector<int>::iterator iterator;
  int d1[11] = {0, 1, 2, 2, 3, 4, 2, 2, 2, 6, 7};
  //
  // Set up a vector.
  //
  vector<int> v1(d1+0, d1+11);
  //
  // Try equal_range variants.
  //
  pair<iterator,iterator> p1 = equal_range(v1.begin(), v1.end(), 3);
  pair<iterator,iterator> p2 = equal_range(v1.begin(), v1.end(), 2, less<int>()); 
  //
  // Output results.
  //
  cout << endl << "The equal range for 3 is: "
       << "( " << *p1.first << " , " 
       << *p1.second << " ) " << endl << endl; 

  cout << endl << "The equal range for 2 is: "
       << "( " << *p2.first << " , " 
       << *p2.second << " ) " << endl; 

  return 0;
}
