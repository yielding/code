#include <iostream>

int main ()
{
  using namespace std;

  locale loc;

  // Get a ctype facet
  const ctype<char>& ct = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
     use_facet<ctype<char> >(loc); 
#else
     use_facet(loc,(ctype<char>*)0); 
#endif

  cout << 'a' << ct.toupper('c') << endl;
 
  return 0;
}

