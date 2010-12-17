#include "stlexam.h"
#include <bitset>

#include <iostream>

int main()
{
  using namespace std;

  bitset<65> b;
  b |= 5;
  cout << b << endl; // results in 00000101
  return 0;
}
