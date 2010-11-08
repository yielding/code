#include <string>
#include <vector>

#include <iostream>
#include "codecvte.h"

int main ()
{
  using namespace std;

  locale loc;  // Default locale

  // Construct new locale using default locale plus
  // user defined codecvt facet
  // This facet converts from ISO Latin 
  // Alphabet No. 1 (ISO 8859-1) to 
  // U.S. ASCII code page 437
  // This facet replaces the default for
  // codecvt<char,char,mbstate_t>
  locale my_loc(loc,new ex_codecvt);

  // imbue modified locale onto cout
  locale old = cout.imbue(my_loc);
  cout << "A \x93 jolly time was had by all" << endl;

  cout.imbue(old);
  cout << "A jolly time was had by all" << endl;

  // Create a vector of strings 
  vector<string> v;
  v.insert(v.begin(),"antelope");
  v.insert(v.begin(),"bison");
  v.insert(v.begin(),"elk");

  copy(v.begin(),v.end(),
      ostream_iterator<string>(cout," "));
  cout << endl;

  // Sort the strings using the locale as a comparitor
  sort(v.begin(),v.end(),loc);

  copy(v.begin(),v.end(),
      ostream_iterator<string>(cout," "));

  cout << endl;
  return 0;
}
