#include <vector>
#include <algorithm>

#include <iostream>

int main()
{
  using namespace std;

  int d1[20] = {17, 3,  5,  -4, 1, 12, -10, -1, 14, 7,
    -6, 8, 15, -11, 2, -2,  18,  4, -3, 0};
  //
  // Set up a vector.
  //
  vector<int> v1(d1+0, d1+20);
  //
  // Output original vector.
  //
  cout << "For the vector: ";
  copy(v1.begin(), v1.end(), ostream_iterator<int>(cout," "));
  //
  // Partial sort the first seven elements.
  //
  partial_sort(v1.begin(), v1.begin()+7, v1.end());

  //
  // Output result.
  //
  cout << endl << endl << "A partial_sort of seven elements gives: " 
       << endl << "     ";

  copy(v1.begin(), v1.end(), ostream_iterator<int>(cout," "));
  cout << endl;

  //
  // A vector of ten elements.
  //
  vector<int> v2(10, 0);
  //
  // Sort the last ten elements in v1 into v2.
  //
  partial_sort_copy(v1.begin()+10, v1.end(), v2.begin(), v2.end());

  //
  // Output result.
  //
  cout << endl << "A partial_sort_copy of the last ten elements gives: " 
       << endl << "     ";
  copy(v2.begin(), v2.end(), ostream_iterator<int>(cout," "));
  cout << endl;

  return 0;
}
