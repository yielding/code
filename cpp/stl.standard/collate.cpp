#include <iostream>
#include <locale> 

int main ()
{
  using namespace std;

  locale loc;
  string s1("blue");
  string s2("blues");

  // Get a reference to the collate<char> facet
  const collate<char>& co =
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
      use_facet<collate<char> >(loc);
#else
      use_facet(loc,(collate<char>*)0);
#endif

  // Compare two strings
  cout << co.compare(s1.begin(),s1.end(),
                     s2.begin(),s2.end()-1) << endl;
  cout << co.compare(s1.begin(),s1.end(),
                     s2.begin(),s2.end()) << endl;

  // Retrieve hash values for two strings
  cout << co.hash(s1.begin(),s1.end()) << endl;
  cout << co.hash(s2.begin(),s2.end()) << endl;

  return 0;
}
