#include <algorithm>
#include <vector>
#include <iostream>

int main ()
{
  using namespace std;
  //
  // Initialize a vector with an array of integers.
  //
  int arr[10] = {1,2,3,4,5,6,7,8,9,10};
  vector<int,allocator<int> > v(arr+0, arr+10);
  //
  // Print out elements in original (sorted) order.
  //
  cout << "Elements before random_shuffle: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
  cout << endl << endl;
  //
  // Mix them up with random_shuffle.
  //
  random_shuffle(v.begin(), v.end());
  //
  // Print out the mixed up elements.
  //
  cout << "Elements after random_shuffle: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl;

  return 0;
}
