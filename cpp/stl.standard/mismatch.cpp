#include <algorithm>
#include <vector>
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  typedef vector<int>::iterator  iterator;

  int d1[4] = {1,2,3,4};
  int d2[4] = {1,3,2,4};
  //
  // Set up two vectors.
  //
  vector<int> vi1(d1+0, d1+4), vi2(d2+0, d2+4);
  //
  // p1 will contain two iterators that point to the first pair of
  // elements that are different between the two vectors.
  //
  pair<iterator, iterator> p1 = mismatch(vi1.begin(), vi1.end(), vi2.begin());	
  //
  // Find the first two elements such that an element in the
  // first vector is greater than the element in the second vector.
  //
  pair<iterator, iterator> p2 = mismatch(vi1.begin(), vi1.end(),
      vi2.begin(), less_equal<int>());
  //
  // Output results.
  //
  cout << *p1.first << ", " << *p1.second << endl;
  cout << *p2.first << ", " << *p2.second << endl;

  return 0;
}
