#include <string>

#include <locale>
#include <iostream>

int main ()
{
  using namespace std;

  typedef ostreambuf_iterator<char,char_traits<char> > iter_type;
  
  locale loc;
  string buffer("10002");
  long double ldval = 10002; 

  // Construct a ostreambuf_iterator on cout
  iter_type begin(cout);

  // Get a money put facet
  const money_put<char,iter_type>& mp = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<money_put<char,iter_type> >(loc);
#else
    use_facet(loc,(money_put<char,iter_type>*)0);
#endif

  // Put out the string representation of the monetary value
  cout << buffer << " --> ";
  mp.put(begin,false,cout,' ',buffer);

  // Put out the long double representation of the monetary value
  cout << endl << ldval << " --> ";
  mp.put(begin,false,cout,' ',ldval);

  cout <<  endl;

  return 0;
}

