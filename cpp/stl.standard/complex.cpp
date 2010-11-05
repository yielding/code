#include <complex>

#include <iostream>

int main () 
{
  using namespace std;
  complex<double> a(1.2, 3.4);
  complex<double> b(-9.8, -7.6);

  a += b;
  a /= sin(b) * cos(a);
  b *= log(a) + pow(b, a);

  cout << "a = " << a << ", b = " << b << endl;

  return 0;
}
