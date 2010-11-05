#include <vector>
#include <string>
#include <boost/algorithm/string.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>

using namespace std;
using namespace boost::posix_time;
using namespace boost::gregorian;
using namespace boost::lambda;

int main()
{
  for (int i=0; i<7; i++)
  {
    date d = day_clock::local_day() + days(i);
    cout << d.day_of_week().as_long_string()  << " ";
    cout << d.day_of_week().as_short_string() << " ";
    cout << d.day_of_week().as_number() << endl;
  }

  ptime now = second_clock::local_time();
  cout << now.time_of_day() << " " ;
  cout << now.time_of_day().hours() << " ";
  cout << now.time_of_day().minutes() << " ";
  cout << now.time_of_day().seconds() << endl;

  ptime m1 = time_from_string("2007-09-28 12:00:45");
  ptime m2 = time_from_string("2007-09-28 13:30:45");

  cout << (m2-m1).hours() * 60 + (m2-m1).minutes();

  /*
  string s="1, 2, 3, 4,  5, 6";
  vector<string> v;
  boost::split(v, s, boost::is_any_of(","));

  for (size_t i=0; i<v.size(); i++)
    cout << atoi(v[i].c_str());

  for_each (v.begin(), v.end(), cout << _1 << ".");
  */
}

