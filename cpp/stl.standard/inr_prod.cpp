#include <numeric>       // For inner_product.
#include <list>          // For list.
#include <vector>        // For vectors.
#include <functional>    // For plus and minus.

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize a list and an int using arrays of ints.
  //
  int a1[3] = {6, -3, -2};
  int a2[3] = {-2, -3, -2};

  list<int>   l(a1+0, a1+3);
  vector<int> v(a2+0, a2+3);
  //
  // Calculate the inner product of the two sets of values.
  //
  int inner_prod = inner_product(l.begin(), l.end(), v.begin(), 0);
  //
  // Calculate a wacky inner product using the same values.
  //
  int wacky = inner_product(l.begin(), l.end(), v.begin(), 0,
      plus<int>(), minus<int>());
  //
  // Print the output.
  //
  cout << "For the two sets of numbers: " << endl << "     ";
  copy(v.begin(),v.end(),
      ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl << " and  ";
  copy(l.begin(),l.end(),
      ostream_iterator<int,char,char_traits<char> >(cout," "));

  cout << "," << endl << endl;
  cout << "The inner product is: " << inner_prod << endl;
  cout << "The wacky result is: " << wacky << endl;

  return 0;
}
