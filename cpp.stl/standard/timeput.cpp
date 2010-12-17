#include <string>
#include <locale>
#include <iostream>

int main ()
{
  using namespace std;

  typedef ostreambuf_iterator<char,char_traits<char> > iter_type;

  locale loc;
  time_t tm = time(NULL);
  struct tm* tmb = localtime(&tm);
  struct tm timeb;
  memcpy(&timeb,tmb,sizeof(struct tm));
  char pat[] = "%c";

  // Get a time_put facet
  const time_put<char,iter_type>& tp = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<time_put<char,iter_type> >(loc);
#else
  use_facet(loc,(time_put<char,iter_type>*)0);
#endif

  // Construct a ostreambuf_iterator on cout
  iter_type begin(cout);

  // Put 
  cout << " --> ";
  tp.put(begin,cout,' ',&timeb,pat,pat+2);

  // Put out 
  cout << endl << " --> ";
  tp.put(begin,cout,' ',&timeb,'c',' ');

  cout <<  endl;

  return 0;
}
