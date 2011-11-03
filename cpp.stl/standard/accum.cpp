#include <numeric>       // For accumulate.
#include <vector>        // For vector.
#include <functional>    // For multiplies.
#include <iostream>

using namespace std;

int main()
{
  typedef vector<int>::iterator iterator;

  //
  // Initialize a vector using an array of integers.
  //
  int d1[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  vector<int> v1(d1+0,  d1+10);
  //
  // Accumulate sums and products.
  //
  int sum  = accumulate(v1.begin(), v1.end(), 0);
  int prod = accumulate(v1.begin(), v1.end(), 1, multiplies<int>());
  //
  // Output the results.
  //
  cout << "For the series: ";
  for (iterator i=v1.begin(); i != v1.end(); ++i)
    cout << *i << " ";

  cout << " where N = 10." << endl;
  cout << "The sum = (N*N + N)/2 = " << sum  << endl;
  cout << "The product = N! = "      << prod << endl;

  return 0;
}
