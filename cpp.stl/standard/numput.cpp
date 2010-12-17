#include <string>
#include <iostream>

int main ()
{
  using namespace std;

  typedef ostreambuf_iterator<char,char_traits<char> > iter_type;

  locale loc;
  bool bval = true;
  long lval = 422432L;
  unsigned long ulval = 12328889UL;
  double dval = 10933.8934; 
  long double ldval = 100028933.8934; 

  // Construct a ostreambuf_iterator on cout
  iter_type begin(cout);

  // Get a num_put facet reference
  const num_put<char,iter_type>& np = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<num_put<char,iter_type> >(loc);
#else
  use_facet(loc,(num_put<char,iter_type>*)0);
#endif

  // Put out a bool
  cout << bval << " --> ";
  np.put(begin,cout,' ',bval);

  // Put out a long
  cout << endl << lval << " --> ";
  np.put(begin,cout,' ',lval);

  // Put out an unsigned long
  cout << endl << ulval << " --> ";
  np.put(begin,cout,' ',ulval);

  // Put out a double
  cout << endl << dval << " --> ";
  np.put(begin,cout,' ',dval);

  // Put out a long double
  cout << endl << ldval << " --> ";
  np.put(begin,cout,' ',ldval);

  cout <<  endl;

  return 0;
}
