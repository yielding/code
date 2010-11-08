#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  typedef vector<int,allocator<int> >::iterator iterator;
  int d1[11] = {0,1,2,2,3,4,2,2,2,6,7};
  //
  // Set up a vector.
  //
  vector<int,allocator<int> > v1(d1+0, d1+11);
  //
  // Try lower_bound variants.
  //
  iterator it1 = lower_bound(v1.begin(),v1.end(),3);
  iterator it2 = lower_bound(v1.begin(),v1.end(),2,less<int>());
  //
  // Try upper_bound variants.
  //
  iterator it3 = upper_bound(v1.begin(),v1.end(),3);
  iterator it4 = upper_bound(v1.begin(),v1.end(),2,less<int>());

  cout << endl << endl
    << "The upper and lower bounds of 3: ( "
    << *it1 << " , " << *it3 << " ]" << endl;

  cout << endl << endl
    << "The upper and lower bounds of 2: ( "
    << *it2 << " , " << *it4 << " ]" << endl;

  return 0;
}
