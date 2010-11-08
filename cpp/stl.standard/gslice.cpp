#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[27] =   {0,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,10};  
  size_t len_buf[3] = {3,3,3};
  size_t stride_buf[3] = {9,3,1};

  // create a valarray of ints
  valarray<int>  vi(ibuf,27);

  // create length and stride valarrays
  valarray<size_t> len(len_buf,3);
  valarray<size_t> stride(stride_buf,3);

  // print out the valarray
  cout << vi << endl;

  // Print out all three dimensions (the entire valarray)
  cout << valarray<int>(vi[gslice(0,len,stride)]) << endl;

  // Print a two dimensional slice out of the middle
  valarray<size_t> len2(2);
  len2[0] = 3;
  len2[1] = 3;
  valarray<size_t> stride2(2);
  stride2[0] = 3;
  stride2[1] = 1;
  cout << valarray<int>(vi[gslice(9,len2,stride2)]) << endl;

  // Print another two dimensional slice out of the middle
  // but orthoganal to one we just did
  stride2[0] = 9;
  stride2[1] = 1;
  cout << valarray<int>(vi[gslice(3,len2,stride2)]) << endl;

  // Print out the last plane in the middle,
  // (orthoganal to both of the previous ones)
  stride2[0] = 3;
  stride2[1] = 9;
  cout << valarray<int>(vi[gslice(1,len2,stride2)]) << endl;

  // Now how about a diagonal slice,
  // upper left front to lower right back
  stride2[0] = 3;
  stride2[1] = 10;
  cout << valarray<int>(vi[gslice(0,len2,stride2)]) << endl;

  return 0;
}
