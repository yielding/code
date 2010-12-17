#include <vector>
#include <algorithm>

#include <iostream>

int main ()
{
  using namespace std;

  int d1[] = {6, 7, 8, 9, 10, 1, 2, 3, 4, 5};
  //
  // Set up a vector.
  //
  vector<int,allocator<int> > v(d1+0, d1+10);
  //
  // Output original vector.
  //
  cout << "For the vector: ";
  copy(v.begin(), v.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  //
  // Swap the first five elements with the last five elements.
  //
  swap_ranges(v.begin(), v.begin()+5, v.begin()+5);
  //
  // Output result.
  //
  cout << endl << endl
       << "Swaping the first five elements with the last five gives: "
       << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  //
  // Now an example of iter_swap -- swap first and last elements.
  //
  iter_swap(v.begin(), v.end()-1);
  //
  // Output result.
  //
  cout << endl << endl
       << "Swaping the first and last elements gives: "
       << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl;

  return 0;
}
