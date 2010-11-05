#include <algorithm>
#include <set>

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize some sets.
  //
  int a1[10] = {1,3,5,7,9,11};
  int a3[4]  = {3,5,7,8};
  set<int> odd(a1+0, a1+6), result, smalll(a3+0, a3+4);
  //
  // Create an insert_iterator for result.
  //
  insert_iterator< set<int> > res_ins(result, result.begin());
  //
  // Demonstrate set_intersection.
  //
  cout << "The result of:" << endl << "{";
  copy(smalll.begin(),smalll.end(), ostream_iterator<int>(cout," "));
  cout << "} intersection {";
  copy(odd.begin(),odd.end(), ostream_iterator<int>(cout," "));
  cout << "} =" << endl << "{";
  set_intersection(smalll.begin(),smalll.end(),odd.begin(),odd.end(),res_ins);
  copy(result.begin(),result.end(), ostream_iterator<int>(cout," "));
  cout << "}" << endl << endl;

  return 0;
}
