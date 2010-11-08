#include <string>
#include <iostream>

int main ()
{
  using namespace std;

  locale loc;

  // Get a numpunct facet
  const numpunct<char>& np = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<numpunct<char> >(loc);
#else
    use_facet(loc,(numpunct<char>*)0);
#endif

  cout << "Decimal point       = " << np.decimal_point() << endl; 
  cout << "Thousands seperator = " << np.thousands_sep() << endl; 
  cout << "True name           = " << np.truename() << endl; 
  cout << "False name          = " << np.falsename() << endl; 

  return 0;
}

