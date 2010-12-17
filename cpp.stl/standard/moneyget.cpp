#include <locale>
#include <sstream>

int main ()
{
  using namespace std;

  typedef istreambuf_iterator<char,char_traits<char> > iter_type;
  
  locale loc;
  string buffer("$100.02");
  string dest;
  long double ldest;
  ios_base::iostate state;
  iter_type end;

  // Get a money_get facet
  const money_get<char,iter_type>& mgf = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<money_get<char,iter_type> >(loc);
#else
    use_facet(loc,(money_get<char,iter_type>*)0);
#endif

  {
    // Build an istringstream from the buffer and construct
    // a beginning iterator on it.
    istringstream ins(buffer);
    iter_type begin(ins);

    // Get a a string representation of the monetary value
    mgf.get(begin,end,false,ins,state,dest);
  }
  {
    // Build another istringstream from the buffer, etc.
    // so we have an iterator pointing to the beginning
    istringstream ins(buffer);
    iter_type begin(ins);

    // Get a a long double representation of the monetary value
    mgf.get(begin,end,false,ins,state,ldest);
  }
  cout << buffer << " --> " << dest << " --> " << ldest << endl;

  return 0;
}



