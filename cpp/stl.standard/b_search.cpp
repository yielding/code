#include <vector>
#include <algorithm>
#include <functional>
#include <iostream>

using namespace std;
ostream_iterator<int> out(cout," ");

int main()
{
  int d1[10] = {0,1,2,2,3,4,2,2,6,7};

  //
  // Set up a vector.
  //
  vector<int> v1(d1+0, d1+10);
  sort(v1.begin(),v1.end());
  //
  // Try binary_search variants.
  //
  bool b1 = binary_search(v1.begin(), v1.end(), 3);
  bool b2 = binary_search(v1.begin(), v1.end(), 11, less<int>());
  //
  // Output results.
  //
  cout << "In the vector: ";
  copy(v1.begin(), v1.end(), out);
  cout << endl << "The number 3 was "  << (b1 ? "FOUND" : "NOT FOUND");
  cout << endl << "The number 11 was " << (b2 ? "FOUND" : "NOT FOUND") << endl;

  return 0;
}
