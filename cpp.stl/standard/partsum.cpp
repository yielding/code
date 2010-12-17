#include <numeric>    // For accumulate.
#include <vector>     // For vector.
#include <functional> // For multiplies.

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize a vector using an array of integers.
  //
  int d1[10] = {1,2,3,4,5,6,7,8,9,10};
  vector<int> v(d1+0, d1+10);
  //
  // Create an empty vectors to store results.
  //
  vector<int> sums((size_t)10), prods((size_t)10);
  //
  // Compute partial_sums and partial_products.
  //
  partial_sum(v.begin(), v.end(), sums.begin());
  partial_sum(v.begin(), v.end(), prods.begin(), multiplies<int>());
  //
  // Output the results.
  //
  cout << "For the series: " << endl << "     ";
  copy(v.begin(),v.end(),ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl << endl;

  cout << "The partial sums: " << endl << "     " ;
  copy(sums.begin(),sums.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout <<" should each equal (N*N + N)/2" << endl << endl;

  cout << "The partial products: " << endl << "     ";
  copy(prods.begin(),prods.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << " should each equal N!" << endl;

  return 0;
}
