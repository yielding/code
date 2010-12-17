#include <numeric>    // For accumulate.
#include <vector>     // For vector.
#include <functional> // For less.

#include <iostream>

ostream_iterator<int> out(cout," ");

int main ()
{
  using namespace std;
  //
  // Initialize a vector using an array of integers.
  //
  int  a1[] = {0,0,0,0,1,0,0,0,0,0};
  char a2[] = "abcdefghji";
  //
  // Create the initial set and copies for permuting.
  //
  vector<int>  m1(a1+0, a1+10); 
  vector<int>  prev_m1((size_t)10), next_m1((size_t)10);
  vector<char> m2(a2+0, a2+10);
  vector<char> prev_m2((size_t)10), next_m2((size_t)10);

  copy(m1.begin(), m1.end(), prev_m1.begin());
  copy(m1.begin(), m1.end(), next_m1.begin());
  copy(m2.begin(), m2.end(), prev_m2.begin());
  copy(m2.begin(), m2.end(), next_m2.begin());
  //
  // Create permutations.
  //
  prev_permutation(prev_m1.begin(), prev_m1.end(), less<int>());
  next_permutation(next_m1.begin(), next_m1.end(), less<int>());
  prev_permutation(prev_m2.begin(), prev_m2.end(), less<int>());
  next_permutation(next_m2.begin(), next_m2.end(), less<int>());
  //
  // Output results.
  //
  cout << "Example 1: " << endl << "     ";
  cout << "Original values:      ";
  copy(m1.begin(), m1.end(), out);
  cout << endl << "     ";
  cout << "Previous permutation: ";
  copy(prev_m1.begin(), prev_m1.end(), out);

  cout << endl<< "     ";
  cout << "Next Permutation:     ";
  copy(next_m1.begin(), next_m1.end(), out);
  cout << endl << endl;

  cout << "Example 2: " << endl << "     ";
  cout << "Original values: ";
  copy(m2.begin(), m2.end(), out);
  cout << endl << "     ";
  cout << "Previous Permutation: ";
  copy(prev_m2.begin(), prev_m2.end(), out);
  cout << endl << "     ";

  cout << "Next Permutation:     ";
  copy(next_m2.begin(), next_m2.end(), out);
  cout << endl << endl;

  return 0;
}
