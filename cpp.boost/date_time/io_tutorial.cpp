#include <iostream>
#include <boost/date_time/local_time/local_time.hpp>

int main()
{
  using namespace boost::gregorian;
  using namespace std;

  for (int i=0; i<1000; i++)
  {
    stringstream ss;
    date_facet* f = new date_facet();
    f->format("%Y.%m.%d");
    ss.imbue(locale(std::locale::classic(), f));
    //ss.str("");
    ss << day_clock::local_day();
    cout << "leech: " << ss.str() << endl;
  }

  return 0;
}
