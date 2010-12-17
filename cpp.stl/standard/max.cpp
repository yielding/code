#include <algorithm>  
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  double  d1 = 10.0, d2 = 20.0;  
  // 
  // Find minimum.
  //
  double val1 = min(d1, d2);                  
  // 
  // The greater comparator returns the greater of the two values.     
  //
  double val2 = min(d1, d2, greater<double>());
  // 
  // Find minimum.
  //
  double val3 = max(d1, d2);                  
  // 
  // The less comparator returns the smaller of the  two values.
  // Note that, like every comparison in the STL, max is 
  // defined in terms of the < operator, so using less here
  // is the same as using the max algorithm with a default
  // comparator.
  //
  double val4 = max(d1, d2, less<double>());

  cout << val1 << " " << val2 << " " << val3 << " " << val4 << endl;

  return 0;
}
