#include <functional>
#include <algorithm>
#include <vector>
#include <iostream>

using namespace std;

int main()
{
  typedef vector<int>::iterator iterator;
  int d1[4] = {1,2,3,4};
  //
  // Set up a vector.
  //
  vector<int> v1(d1+0, d1+4);
  //
  // Create an 'equal to 3' unary predicate by binding 3 to
  // the equal_to binary predicate.
  //
  binder1st<equal_to<int> > equal_to_3 = bind1st(equal_to<int>(),3);
  // 
  // Now use this new predicate in a call to find_if.
  //
  iterator it1 = find_if(v1.begin(),v1.end(),equal_to_3);
  //
  // Even better, construct the new predicate on the fly.
  //
  iterator it2 = find_if(v1.begin(),v1.end(),bind1st(equal_to<int>(),3)); 
  //
  // And now the same thing using bind2nd.
  // Same result since == is commutative.
  //
  iterator it3 = find_if(v1.begin(),v1.end(),bind2nd(equal_to<int>(),3)); 
  //
  // Output results.
  //
  cout << *it1 << " " << *it2 << " " << *it3 << endl;

  return 0;
}
