#include <algorithm>
#include <vector>	
#include <functional>

#include <iostream>

int main ()
{
  using namespace std;

  int d1[4] = {1,2,3,4};
  int d2[8] = {11,13,15,17,12,14,16,18};
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+4), v2(d1+0, d1+4);
  //
  // Set up four destination vectors.
  //
  vector<int> v3(d2+0, d2+8), v4(d2+0, d2+8), v5(d2+0, d2+8), v6(d2+0, d2+8);
  //
  // Set up one empty vector.
  //
  vector<int> v7;
  //
  // Merge v1 with v2.
  //
  merge(v1.begin(), v1.end(), v2.begin(), v2.end(), v3.begin());
  //
  // Now use comparator.
  //
  merge(v1.begin(), v1.end(), v2.begin(), v2.end(), v4.begin(), less<int>());
  //
  // In place merge v5.
  //
  vector<int>::iterator mid = v5.begin();
  advance(mid,4);
  inplace_merge(v5.begin(),mid,v5.end());
  //
  // Now use a comparator on v6.
  //
  mid = v6.begin();
  advance(mid,4);
  inplace_merge(v6.begin(), mid, v6.end(), less<int>());	  
  // 
  // Merge v1 and v2 to empty vector using insert iterator.
  //
  merge(v1.begin(), v1.end(), v2.begin(), v2.end(), back_inserter(v7));
  //
  // Copy all cout.
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
  copy(v5.begin(),v5.end(),out);
  cout << endl;
  copy(v6.begin(),v6.end(),out);
  cout << endl;
  copy(v7.begin(),v7.end(),out);
  cout << endl;
  // 
  // Merge v1 and v2 to cout.
  //
  merge(v1.begin(),v1.end(),v2.begin(),v2.end(),
      ostream_iterator<int,char,char_traits<char> >(cout," "));

  return 0;
}
