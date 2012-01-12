#include "char_field.h"
#include "bin_field.h"

#include <iostream>

using namespace std;

int main()
{
  char_field n0("000012345", "123456789");
  bin_field  n1("123456789", "1");

  cout << n0.byte_count() << endl;
  cout << n1.byte_count() << endl;
}
