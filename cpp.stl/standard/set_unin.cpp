#include <algorithm>
#include <set>

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize some sets.
  //
  int a2[6]  = {2,4,6,8,10,12};
  int a3[4]  = {3,5,7,8};
  set<int>  even(a2+0, a2+6), result, smalll(a3+0, a3+4);
  //
  // Create an insert_iterator for result.
  //
  insert_iterator< set<int> > res_ins(result, result.begin());
  //
  // Demonstrate set_union.
  //
  cout << "The result of:" << endl << "{";
  copy(smalll.begin(),smalll.end(), ostream_iterator<int>(cout," "));
  cout << "} union {";
  copy(even.begin(),even.end(), ostream_iterator<int>(cout," "));
  cout << "} =" << endl << "{";
  set_union(smalll.begin(), smalll.end(), even.begin(), even.end(), res_ins);
  copy(result.begin(),result.end(), ostream_iterator<int>(cout," "));
  cout << "}" << endl << endl;

  return 0;
}
