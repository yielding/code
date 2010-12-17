#include <algorithm>
#include <set>
#include <iostream>

using namespace std;

ostream_iterator<int> out(cout, " ");

int main ()
{
  //
  // Initialize some sets.
  //
  int a1[10] = {1,2,3,4,5,6,7,8,9,10};
  int a2[6]  = {2,4,6,8,10,12};
  int a3[4]  = {3,5,7,8};
  set<int> all(a1+0, a1+10), even(a2+0, a2+6), smalll(a3+0, a3+4);

  //
  // Demonstrate includes.
  //
  cout << "The set: ";
  copy(all.begin(),all.end(), out);

  bool answer = includes(all.begin(), all.end(), smalll.begin(), smalll.end());
  cout << endl << (answer ? "INCLUDES " : "DOES NOT INCLUDE ");
  copy(smalll.begin(),smalll.end(), out);

  answer = includes(all.begin(), all.end(), even.begin(), even.end());
  cout << ", and" << endl << (answer ? "INCLUDES" : "DOES NOT INCLUDE ");

  copy(even.begin(),even.end(), out);

  cout << endl << endl;

  return 0;
}
