#include <functional>
#include <deque>
#include <algorithm>

#include <iostream>
#include <iomanip>

int main ()
{
  using namespace std;

  //
  // Initialize a deque with an array of integers.
  //
  int arr1[5] = {99, 264, 126, 330, 132};
  int arr2[5] = {280, 105, 220, 84, 210};
  deque<int,allocator<int> > d1(arr1+0, arr1+5), d2(arr2+0, arr2+5);
  //
  // Print the original values.
  //
  cout << "The following pairs of numbers: " << endl << "     ";
  deque<int,allocator<int> >::iterator i1;
  for (i1 = d1.begin(); i1 != d1.end(); i1++)
    cout << setw(6) << *i1 << " ";
  cout << endl << "     ";
  for (i1 = d2.begin(); i1 != d2.end(); i1++)
    cout << setw(6) << *i1 << " ";
  //
  // Transform the numbers in the deque to their 
  // factorials and store in the vector.
  //
  transform(d1.begin(), d1.end(), d2.begin(), d1.begin(), multiplies<int>());
  //
  // Display the results.
  //
  cout << endl << endl;
  cout << "Have the products: " << endl << "     ";
  for (i1 = d1.begin(); i1 != d1.end(); i1++)
    cout << setw(6) << *i1 << " ";
  cout << endl;

  return 0;
}
