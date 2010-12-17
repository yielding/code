#include <sstream>
#include "codecvte.h"

int main ()
{
  using namespace std;

  mbstate_t state;

  // three strings to use as buffers
  string ins("\xfc \xcc \xcd \x61 \xe1 \xd9 \xc6 \xe6 \xf5");
  string ins2(ins.size(),'.');
  string outs(ins.size()/ex_codecvt().encoding(),'.');

  // Print initial contents of buffers
  cout << "Before:\n" << ins << endl;
  cout << ins2 << endl;
  cout << outs << endl << endl;

  // Initialize buffers
  string::iterator in_it = ins.begin();
  string::iterator out_it = outs.begin();
  string::const_iterator const_in_it = ins.begin();
  string::const_iterator const_out_it = outs.begin();

  // Create a user defined codecvt fact
  // This facet converst from ISO Latin 
  // Alphabet No. 1 (ISO 8859-1) to 
  // U.S. ASCII code page 437
  // This facet replaces the default for
  // codecvt<char,char,mbstate_t>
  locale loc(locale(),new ex_codecvt);

  // Now get the facet from the locale 
  const codecvt<char,char,mbstate_t>& cdcvt = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<codecvt<char,char,mbstate_t> >(loc);
#else
  use_facet(loc,(codecvt<char,char,mbstate_t>*)0);
#endif

  // convert the buffer
  cdcvt.in(state,ins.begin(),ins.end(),const_in_it,
      outs.begin(),outs.end(),out_it);

  cout << "After in:\n" << ins << endl;
  cout << ins2 << endl;
  cout << outs << endl << endl;

  // Lastly, convert back to the original codeset
  in_it = ins.begin();
  out_it = outs.begin();
  cdcvt.out(state, outs.begin(),outs.end(),const_out_it,
      ins2.begin(),ins2.end(),in_it);

  cout << "After out:\n" << ins << endl;
  cout << ins2 << endl;
  cout << outs << endl;

  return 0;
}
