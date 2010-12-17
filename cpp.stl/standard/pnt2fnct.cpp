#include<functional>
#include<deque>
#include<vector>
#include<algorithm>

#include <iostream>

using namespace std;
//
// Create a function.
//
int factorial (int x)
{
  int result = 1;
  for(int i = 2; i <= x; i++)
    result *= i;
  return result;
}

int main ()
{
  //
  // Initialize a deque with an array of integers.
  //
  int init[7] = {1,2,3,4,5,6,7};
  deque<int> d(init+0, init+7);
  //
  // Create an empty vector to store the factorials.
  //
  vector<int> v((size_t)7);
  //
  // Transform the numbers in the deque to their factorials and store
  // in the vector.
  //
  transform(d.begin(), d.end(), v.begin(), ptr_fun(factorial));
  //
  // Print the results.
  //
  cout << "The following numbers: " << endl << "     ";
  copy(d.begin(), d.end(), ostream_iterator<int>(cout," "));

  cout << endl << endl;
  cout << "Have the factorials: " << endl << "     ";
  copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
  cout << endl;

  return 0;
}
