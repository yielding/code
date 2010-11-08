#include <algorithm>
#include <vector>

#include <iostream>

int main ()
{
  using namespace std;
  //
  // Initialize a vector with an array of integers.
  //
  int arr[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  vector<int> v(arr+0, arr+10);
  //
  // Print out elements in original (sorted) order.
  //
  cout << "Elements before rotate: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
  cout << endl << endl;
  //
  // Rotate the elements.
  //
  rotate(v.begin(), v.begin()+4, v.end());

  //
  // Print out the rotated elements.
  //
  cout << "Elements after rotate: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
  cout << endl;

  return 0;
}
