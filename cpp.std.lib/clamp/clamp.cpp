#include <iostream>
#include <algorithm>

using namespace std;

int main(int argc, char *argv[])
{
  const int lo = -32768;
  const int hi =  32767;

  cout << clamp( 12000, lo, hi) << endl;
  cout << clamp(-36000, lo, hi) << endl;
  cout << clamp( 40000, lo, hi) << endl;

  return 0;
}
