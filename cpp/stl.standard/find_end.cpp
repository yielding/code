#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

int main()
{
  using namespace std;

  typedef vector<int>::iterator iterator;
  int d1[10] = {0,1,6,5,3,2,2,6,5,7};
  int d2[4] = {6,5,0,0};
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+10), v2(d2+0, d2+2);
  //
  // Try both find_first_of variants.
  //
  iterator it1 = find_first_of(v1.begin(), v1.end(), v2.begin(), v2.end()); 

  iterator it2 = find_first_of(v1.begin(), v1.end(), v2.begin(), v2.end(), equal_to<int>());

  //
  // Try both find_end variants.
  //
  iterator it3 = find_end(v1.begin(), v1.end(), v2.begin(), v2.end()); 

  iterator it4 = find_end(v1.begin(), v1.end(), v2.begin(), v2.end(), equal_to<int>());
  //
  // Output results of find_first_of.
  // Iterator now points to the first element that matches one of a set of values 
  //
  if (it3 == it4 && it1 == it2)
  {
    cout << "For the vectors: ";
    copy(v1.begin(),v1.end(), 
        ostream_iterator<int,char,char_traits<char> >(cout," " ));
    cout << " and ";
    copy(v2.begin(),v2.end(), 
        ostream_iterator<int,char,char_traits<char> >(cout," " ));
    cout << endl << endl
      << "both versions of find_first_of point to: " << *it1 << endl;
    //
    // Output results of find_end.
    // Iterator now points to the first element of the last find subsequence.
    //  
    cout << endl << endl
      << "both versions of find_end point to: " << *it3 << endl;
  }

  return 0;
}
