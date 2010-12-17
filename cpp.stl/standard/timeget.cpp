#include <string>

#include <locale>
#include <sstream>
#include <time.h>

using namespace std;

// Print out a tm struct
ostream& operator<< (ostream& os, const struct tm& t)
{
  os << "Daylight Savings = " << t.tm_isdst << endl;
  os << "Day of year      = " << t.tm_yday << endl;
  os << "Day of week      = " << t.tm_wday << endl;
  os << "Year             = " << t.tm_year << endl;
  os << "Month            = " << t.tm_mon << endl;
  os << "Day of month     = " << t.tm_mday << endl;
  os << "Hour             = " << t.tm_hour << endl;
  os << "Minute           = " << t.tm_min << endl;
  os << "Second           = " << t.tm_sec << endl;
  return os;
}

int main ()
{
  typedef istreambuf_iterator<char> iter_type;

  locale loc;
  time_t tm = time(NULL);
  struct tm* tmb = localtime(&tm);
  struct tm timeb;
  memcpy(&timeb,tmb,sizeof(struct tm));
  ios_base::iostate state;
  iter_type end;

  // Get a time_get facet
  const time_get<char,iter_type>& tg = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<time_get<char,iter_type> >(loc);
#else
  use_facet(loc,(time_get<char,iter_type>*)0);
#endif

  cout << timeb << endl;
  {
    // Build an istringstream from the buffer and construct
    // beginning and ending iterators on it.
    istringstream ins("12:46:32");
    iter_type begin(ins);

    // Get the time
    tg.get_time(begin,end,ins,state,&timeb);
  }
  cout << timeb << endl;
  {
    // Get the date
    istringstream ins("Dec 6 1996");
    iter_type begin(ins);
    tg.get_date(begin,end,ins,state,&timeb);
  }
  cout << timeb << endl;
  {
    // Get the weekday
    istringstream ins("Tuesday");
    iter_type begin(ins);
    tg.get_weekday(begin,end,ins,state,&timeb);
  }
  cout << timeb << endl;
  return 0;
}
