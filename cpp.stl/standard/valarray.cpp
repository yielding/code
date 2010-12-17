#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[10] = {0,1,2,3,4,5,6,7,8,9};  
  int ibuf2[10] = {10,11,12,13,14,15,16,17,18,19};  

  // create 2 valarrays of ints
  valarray<int>         vi(ibuf,10);
  valarray<int>         vi2(ibuf2,10);

  // print them out
  cout << vi << endl << vi2 << endl;

  vi += vi2;
  vi2 *= 2;
  valarray<int> vi3 = vi2 % vi;

  // print them out again
  cout << vi << endl << vi2 << endl << vi3 << endl;

  return 0;
}
