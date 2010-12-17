#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[27] = {0,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,10};  
  int buf13[9] = {13,13,13,13,13,13,13,13,13};
  size_t len_buf[3] = {3,3,3};
  size_t stride_buf[3] = {9,3,1};

  // create a valarray of ints
  valarray<int>  vi(ibuf,27);

  // print out the valarray
  cout << vi << endl;

  // Get a two dimensional diagonal slice out of the middle
  valarray<size_t> len2(2);
  len2[0] = 3;
  len2[1] = 3;
  valarray<size_t> stride2(2);
  stride2[0] = 3;
  stride2[1] = 10;
  gslice_array<int> gsl = vi[gslice(0,len2,stride2)];

  // print out the slice
  cout << gsl << endl;

  // Assign 13?s to everything in the slice
  gsl = valarray<int>(buf13,9);

  // print out the slice and our original valarray
  cout << gsl << endl << vi <<  endl;

  return 0;
}

