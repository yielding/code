#include "valarray.h"  // Contains a valarray stream inserter

int main(void)
{
  using namespace std;

  int ibuf[10] = {0,1,2,3,4,5,6,7,8,9};
  size_t sbuf[6] = {0,2,3,4,7,8};

  // create a valarray of ints
  valarray<int>         vi(ibuf,10);

  // create a valarray of indices for a selector
  valarray<size_t> selector(sbuf,6);

  // print out the valarray<int>
  cout << vi << endl;

  // Get a indirect_array 
  // and assign that indirect to another valarray
  indirect_array<int> select = vi[selector];
  valarray<int> vi3 = select;

  // print out the selective array
  cout << vi3 << endl;

  // Double the selected values
  select += vi3;

  // print out vi1 again
  cout << vi << endl;

  return 0;
}
