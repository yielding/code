#include <string>
#include <sstream>

int main ()
{
  using namespace std;

  typedef istreambuf_iterator<char,char_traits<char> > iter_type;
  
  locale loc;
  ios_base::iostate state;
  bool bval = false;
  long lval = 0L;
  long double ldval = 0.0;
  iter_type end;

  // Get a num_get facet
  const num_get<char,iter_type>& tg = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<num_get<char,iter_type> >(loc);
#else
    use_facet(loc,(num_get<char,iter_type>*)0);
#endif
#ifndef _RWSTD_NO_BOOL
  {
    // Build an istringstream from the buffer and construct
    // beginning and ending iterators on it.
    istringstream ins("true");
    iter_type begin(ins);

    // Get the time
    tg.get(begin,end,ins,state,bval);
  }
#endif
  cout << bval << endl;
  {
    // Get the date
    istringstream ins("2422235");
    iter_type begin(ins);
    tg.get(begin,end,ins,state,lval);
  }
  cout << lval << endl;
  {
    // Get the weekday
    istringstream ins("32324342.98908");
    iter_type begin(ins);
    tg.get(begin,end,ins,state,ldval);
  }
  cout << ldval << endl;

  return 0;
}

