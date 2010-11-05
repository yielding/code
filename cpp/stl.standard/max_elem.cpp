#include <algorithm>
#include <vector>
#include <functional>

#include <iostream>

using namespace std;

int main()
{
  typedef vector<int>::iterator iterator;							   	 
  int d1[5] = {1,3,5,32,64}; 			 				
  // 
  // Set up vector.
  //
  vector<int> v1(d1+0, d1+5);	 
  //
  // Find the largest element in the vector.
  //
  iterator it1 = max_element(v1.begin(), v1.end());
  // 
  // Find the largest element in the range from
  // the beginning of the vector to the 2nd to last.
  //
  iterator it2 = max_element(v1.begin(), v1.end()-1, less<int>());   
  // 
  // Find the smallest element.
  //
  iterator it3 = min_element(v1.begin(), v1.end());  
  // 
  // Find the smallest value in the range from
  // the beginning of the vector plus 1 to the end.
  //
  iterator it4 = min_element(v1.begin()+1, v1.end(), less<int>());      

  cout << *it1 << " " << *it2 << " " << *it3 << " " << *it4 << endl;

  return 0;
}
