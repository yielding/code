#include <vector>
#include <list>
#include <algorithm>
#include <numeric>

#include <iostream>

using namespace std;

int square (int n) { return n * n; }

class iotaGen
{
public:
  iotaGen (int iv) : current(iv) { }
  int operator () () { return current++; }
private:
  int current;
};

//
// Illustrate the use of the transform algorithm.
//

void transform_example ()
{
  //
  // Generate a list of values from 1 to 6.
  //
  list<int> aList;
  generate_n(inserter(aList, aList.begin()), 6, iotaGen(1));
  cout << "Original list: ";
  copy(aList.begin(), aList.end(), ostream_iterator<int,char>(cout, " "));
  cout << endl;

  //
  // Transform elements by squaring, copy into vector.
  //
  vector<int> aVec(6);
  transform (aList.begin(), aList.end(), aVec.begin(), square);
  cout << "After squaring: ";
  copy(aVec.begin(), aVec.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Transform vector again, in place, yielding 4th powers.
  //
  transform (aVec.begin(), aVec.end(), aVec.begin(), square);
  cout << "After squaring again: ";
  copy(aVec.begin(), aVec.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Transform in parallel, yielding cubes.
  //
  vector<int> cubes(6);
  transform (aVec.begin(), aVec.end(), aList.begin(), cubes.begin(),
      divides<int>());
  cout << "After division: ";
  copy(cubes.begin(), cubes.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}

//
// Illustrate the use of the partial sum algorithm.
//

void partial_sum_example ()
{
  //
  // Generate values 1 to 5.
  //
  vector<int> aVec(5);
  generate (aVec.begin(), aVec.end(), iotaGen(1));
  //
  // Output partial sums.
  //
  cout << "Partial sums examples" << endl;
  cout << "Partial sums : ";
  partial_sum (aVec.begin(), aVec.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //    
  // Output partial products.
  //
  cout << "Partial products: ";
  partial_sum (aVec.begin(), aVec.end(), 
      ostream_iterator<int>(cout, " "),
      multiplies<int>() );

  cout << endl;
}

//
// Illustrate the use of the adjacent difference algorithm.
//

void adjacent_difference_example ()
{
  //
  // Generate values 1 to 5.
  //
  vector<int> aVec(5);
  generate(aVec.begin(), aVec.end(), iotaGen(1));
  //
  // Output partial sums.
  //
  cout << "Adjacent Differences examples" << endl;
  cout << "Adjacent Differences : ";
  adjacent_difference (aVec.begin(), aVec.end(),
      ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Output partial products.
  //
  cout << "Adjacent sums: ";
  adjacent_difference(aVec.begin(), aVec.end(),
      ostream_iterator<int>(cout, " "), plus<int>());

  cout << endl;
}

int main ()
{
  cout << "STL generic algorithms -- that transform sequences"  << endl;

  transform_example();
  partial_sum_example();
  adjacent_difference_example ();

  cout << "End generic transform algorithms example" << endl;

  return 0;
}
