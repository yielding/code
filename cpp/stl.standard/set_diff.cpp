#include <algorithm>
#include <set>

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize some sets.
  //
  int a1[10] = {1,2,3,4,5,6,7,8,9,10};
  int a2[6]  = {2,4,6,8,10,12};

  typedef set<int> IntSet;

  IntSet all(a1+0, a1+10), even(a2+0, a2+6), odd;
  //
  // Create an insert_iterator for odd.
  //
  insert_iterator<IntSet> odd_ins(odd, odd.begin());
  //
  // Demonstrate set_difference.
  //
  cout << "The result of:" << endl << "{";
  copy(all.begin(),all.end(), ostream_iterator<int>(cout," "));
  cout << "} - {";
  copy(even.begin(),even.end(), ostream_iterator<int>(cout," "));
  cout << "} =" << endl << "{";
  set_difference(all.begin(), all.end(), even.begin(), even.end(), odd_ins);
  copy(odd.begin(),odd.end(), ostream_iterator<int>(cout," "));
  cout << "}" << endl << endl;

  return 0;
}
