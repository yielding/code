#include <algorithm>
#include <vector>
 
#include <iostream>

int main ()
{
  using namespace std;
  //
  // Initialize a vector with an array of integers.
  //
  int arr[10] = { 1,2,3,4,5,6,7,8,9,10 };
  vector<int> v(arr+0, arr+10);
  //
  // Print out elements in original (sorted) order.
  //
  cout << "Elements before reverse: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
  cout << endl << endl;
  //
  // Reverse the ordering.
  //
  reverse(v.begin(), v.end());
  //
  // Print out the reversed elements.
  //
  cout << "Elements after reverse: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
  cout << endl << endl;

  cout << "A reverse_copy to cout: " << endl << "     ";
  reverse_copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
  cout << endl;

  return 0;
}
