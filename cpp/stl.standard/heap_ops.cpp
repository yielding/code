#include <algorithm>
#include <vector>
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  int d1[4] = {1,2,3,4};
  int d2[4] = {1,3,2,4};				   
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+4), v2(d2+0, d2+4);
  //
  // Make heaps.
  //
  make_heap(v1.begin(), v1.end());
  make_heap(v2.begin(), v2.end(), less<int>());
  //
  // v1 = (4,x,y,z)  and  v2 = (4,x,y,z)
  //
  // Note that x, y and z represent the remaining values in the
  // container (other than 4).  The definition of the heap and heap
  // operations  does not require any particular ordering
  // of these values.
  //
  // Copy both vectors to cout.
  //
  ostream_iterator<int> out(cout," ");
  copy(v1.begin(), v1.end(), out);
  cout << endl;
  copy(v2.begin(), v2.end(), out);
  cout << endl;
  //
  // Now let's pop.
  //
  pop_heap(v1.begin(), v1.end());
  pop_heap(v2.begin(), v2.end(), less<int>());
  //
  // Copy both vectors to cout.
  //
  copy(v1.begin(), v1.end(), out);
  cout << endl;
  copy(v2.begin(), v2.end(), out);
  cout << endl;
  // 
  // And push.
  //
  push_heap(v1.begin(), v1.end());
  push_heap(v2.begin(), v2.end(), less<int>());
  //
  // Copy both vectors to cout.
  //
  copy(v1.begin(),v1.end(),out);
  cout << endl;
  copy(v2.begin(),v2.end(),out);
  cout << endl;
  //
  // Now sort those heaps.
  //
  sort_heap(v1.begin(), v1.end());
  sort_heap(v2.begin(), v2.end(), less<int>());
  // 
  // Copy both vectors to cout.
  //
  copy(v1.begin(), v1.end(), out);
  cout << endl;
  copy(v2.begin(), v2.end(), out);
  cout << endl;

  return 0;
}
