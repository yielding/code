#include <complex>
#include <utility>

#include <iostream>

using namespace std;

typedef complex<double> dcomplex;

//
// Return roots of a quadratic equation.
//

pair<dcomplex, dcomplex> 
quadratic(dcomplex a, dcomplex b, dcomplex c)
{
  dcomplex root = sqrt(b * b - 4.0 * a * c);
  a = a * 2.0;

  return make_pair((-b + root) / a, (-b - root) / a);
}

int main()
{
  dcomplex a(2, 3);
  dcomplex b(4, 5);
  dcomplex c(6, 7);

  pair<dcomplex, dcomplex> ans = quadratic(a, b, c);

  cout << "Roots are " << ans.first << " and " << ans.second  << endl;

  return 0;
}
