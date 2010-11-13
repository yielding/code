#include <algorithm>
#include <vector>	
#include <iostream>

int main ()
{
  using namespace std;

  int d1[4] = {1,2,3,4};
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+4), v2(d1+0, d1+4);
  //
  // Set up one empty vector.
  //
  vector<int> v3;
  //
  // Fill all of v1 with 9.
  //
  fill(v1.begin(), v1.end(), 9);
  //
  // Fill first 3 of v2 with 7.
  //
  fill_n(v2.begin(), 3, 7);
  //
  // Use insert iterator to fill v3 with 5 11's.
  //
  fill_n(back_inserter(v3), 5, 11);
  //
  // Copy all three to cout.
  //
  ostream_iterator<int> out(cout," ");
  copy(v1.begin(),v1.end(),out);
  cout << endl;
  copy(v2.begin(),v2.end(),out);
  cout << endl;
  copy(v3.begin(),v3.end(),out);
  cout << endl;
  //
  // Fill cout with 3 5's.
  //
  fill_n(ostream_iterator<int>(cout," "), 3, 5);
  cout << endl;

  return 0;
}
