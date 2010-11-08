#include <algorithm>
#include <vector>
#include <iterator>
#include <functional>

#include <iostream>

using namespace std;

template<class Arg>
struct not_zero : public unary_function<Arg, bool>
{
  bool operator() (const Arg& a) { return a != 0; }
};

int main ()
{
  int arr[10] = {1,2,3,4,5,6,7,8,9,10};
  vector<int> v(arr+0, arr+10);

  copy(v.begin(),v.end(),ostream_iterator<int>(cout," "));
  cout << endl << endl;
  //
  // Remove the 7.
  //
  vector<int>::iterator result = remove(v.begin(), v.end(), 7);
  //
  // Delete dangling elements from the vector.
  //
  v.erase(result, v.end());

  copy(v.begin(),v.end(),ostream_iterator<int>(cout," "));
  cout << endl << endl;
  //
  // Remove everything beyond the fourth element.
  //
  result = remove_if(v.begin()+4, v.begin()+8, not_zero<int>());
  //
  // Delete dangling elements.
  //
  v.erase(result, v.end());

  copy(v.begin(),v.end(),ostream_iterator<int>(cout," "));
  cout << endl << endl;
  //
  // Now remove all 3s on output.
  // 
  remove_copy(v.begin(), v.end(), ostream_iterator<int>(cout," "), 3);
  cout << endl << endl;
  //
  // Now remove everything satisfying predicate on output.
  // Should yield a NULL vector.
  //
  remove_copy_if(v.begin(), v.end(), ostream_iterator<int>(cout," "),
      not_zero<int>());

  return 0;
}
