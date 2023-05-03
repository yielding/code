#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  int a = 1;
  int b, c;

  (a == 1) 
    ?  b = a+1, c = b*2 
    :  b = 0, c = 0 ;

  cout << b << c << endl;

  return 0;
}
