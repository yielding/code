#include <algorithm>
#include <vector>	
#include <iostream>

using namespace std;

int main()
{
  int d1[4] = {1, 2, 3, 4};
  int d2[4] = {5, 6, 7, 8};
  //
  // Set up three vectors.
  //
  vector<int> v1(d1+0, d1+4), v2(d2+0, d2+4), v3(d2+0, d2+4);
  //
  // Set up one empty vector.
  //
  vector<int> v4;
  //
  // Copy v1 to v2.
  //
  copy(v1.begin(), v1.end(), v2.begin());
  //
  // Copy backwards v1 to v3.
  //
  copy_backward(v1.begin(), v1.end(), v3.end());
  //
  // Use insert iterator to copy into empty vector.
  //
  copy(v1.begin(), v1.end(), back_inserter(v4));
  //
  // Copy all four to cout.
  //
  ostream_iterator<int> out(cout," ");

  copy(v1.begin(),v1.end(),out);
  cout << endl;

  copy(v2.begin(),v2.end(),out);
  cout << endl;

  copy(v3.begin(),v3.end(),out);
  cout << endl;

  copy(v4.begin(),v4.end(),out);
  cout << endl;

  return 0;
}
