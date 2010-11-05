// ref_count(2)
#include <algorithm>
#include <vector>
 
#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize two vectors.
  //
  int a1[20] = {4, 5, 5, 9, -1, -1, -1, 3, 7, 5, 
                5, 5, 6, 7, 7, 7, 4, 2, 1, 1};
  vector<int> v(a1+0, a1+20), result;

  //
  // Create an insert_iterator for results.
  //
  insert_iterator< vector<int> > ins(result, result.begin());

  //
  // Demonstrate includes.
  //
  cout << "The vector: " << endl << "    ";
  copy(v.begin(), v.end(),
      ostream_iterator<int>(cout," "));

  //
  // Find the unique elements.
  //
  unique_copy(v.begin(), v.end(), 
      ins);
  //
  // Display the results.
  //
  cout << endl << endl << "Has the following unique elements:"
       << endl << "     ";

  copy(result.begin(),result.end(), 
      ostream_iterator<int>(cout," "));
  cout << endl;

  return 0;
}
