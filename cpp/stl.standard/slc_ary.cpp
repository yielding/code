#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[10] = {0,1,2,3,4,5,6,7,8,9};
  int ibuf2[5] = {1,3,5,7,9};

  // create a valarray of ints
  valarray<int>         vi(ibuf,10);
  valarray<int>         vi2(ibuf2,5);

  // print it out
  cout << vi << endl << vi2 << endl;

  // Get a slice and assign that slice to another array
  slice_array<int> sl = vi[slice(1,5,2)];
  valarray<int> vi3 = sl;

  // print out the slice
  cout << vi3 << endl;

  // Add slice from vi2 to slice of vi1
  sl += vi2;

  // print out vi1 again
  cout << vi << endl;

  return 0;
}
