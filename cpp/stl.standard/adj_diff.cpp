#include <numeric>       // For adjacent_difference.
#include <vector>        // For vector.
#include <functional>    // For multiplies.

#include <iostream>

using namespace std;

ostream_iterator< int, char, char_traits<char> > out(cout, " ");

int main ()
{
   //
   // Initialize a vector of ints from an array.
   //
   int arr[10] = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55};
   vector<int> v(arr+0,  arr+10);
   //
   // Two uninitialized vectors for storing results.
   //
   vector<int> diffs(10),  prods(10);
   //
   // Calculate difference(s) using default operator (minus).
   //
   adjacent_difference(v.begin(), v.end(), diffs.begin());
   // 
   // Calculate difference(s) using the times operator.
   //
   adjacent_difference(v.begin(),  v.end(),  prods.begin(),  multiplies<int>());
   //
   // Output the results.
   //
   cout << "For the vector: " << endl << "     ";
   copy(v.begin(), v.end(), out);
   cout << endl << endl;

   cout << "The differences between adjacent elements are: " << endl << "    ";
   copy(diffs.begin(), diffs.end(), out); 
   cout << endl << endl;

   cout << "The products of adjacent elements are: " << endl << "     ";
   copy(prods.begin(), prods.end(), out);
   cout << endl;

   return 0;
}
