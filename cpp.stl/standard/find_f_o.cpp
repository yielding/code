#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

int main()
{
  using namespace std;

  typedef vector<int>::iterator iterator;
  int d1[10] = {0,1,2,2,3,4,2,2,6,7};
  int d2[2] = {6,4};
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+10), v2(d2+0, d2+2);
  //
  // Try both find_first_of variants.
  //
  iterator it1 = find_first_of(v1.begin(), v1.end(), v2.begin(), v2.end()); 

  find_first_of(v1.begin(), v1.end(), v2.begin(), v2.end(), equal_to<int>());
  //
  // Output results.
  //
  cout << "For the vectors: ";
  copy(v1.begin(),v1.end(), ostream_iterator<int>(cout," " ));
  cout << " and ";
  copy(v2.begin(),v2.end(), ostream_iterator<int>(cout," " ));
  cout << endl << endl
       << "both versions of find_first_of point to: "  << *it1;
  cout << endl; 

  return 0;
}
