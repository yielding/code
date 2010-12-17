#include <iostream>

int main ()
{
  using namespace std;

  locale loc;

#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
  cout << has_facet<ctype<char> >(loc) << endl;
#else
  cout << has_facet(loc,(ctype<char>*)0) << endl;
#endif

  return 0;
}

