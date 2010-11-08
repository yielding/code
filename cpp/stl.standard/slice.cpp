#include "valarray.h"

int main(void)
{
  using namespace std;

  int ibuf[10] = {0,1,2,3,4,5,6,7,8,9};  

  // create a valarray of ints
  valarray<int>         vi(ibuf,10);

  // print it out
  cout << vi << endl;

  // print out a slice
  cout << valarray<int>(vi[slice(1,3,2)]) << endl;

  return 0;
}
