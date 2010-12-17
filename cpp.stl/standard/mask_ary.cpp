#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[10] = {0,1,2,3,4,5,6,7,8,9};
  bool mbuf[10] = {1,0,1,1,1,0,0,1,1,0};

  // create a valarray of ints
  valarray<int>         vi(ibuf,10);

  // create a valarray of bools for a mask
  valarray<bool> mask(mbuf,10);

  // print out the valarray<int>
  cout << vi << endl;

  // Get a mask array and assign that mask to another array
  mask_array<int> msk = vi[mask];
  valarray<int> vi3 = msk;

  // print out the masked_array
  cout << vi3 << endl;

  // Double the masked values
  msk += vi3;

  // print out vi1 again
  cout << vi << endl;

  return 0;
}
