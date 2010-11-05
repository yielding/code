#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/format.hpp>
#include <boost/progress.hpp>

#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

using namespace boost;
using namespace std;
using namespace boost::posix_time;
using namespace boost::gregorian;

void date1(int& y, int&m, int& d, int& w, int& h, int& n, int& s)
{
  string const& st = to_iso_extended_string(second_clock::local_time());
  vector<string> arr;
  boost::split(arr, st, boost::is_any_of("T-:"));

  s = atoi(arr[5].c_str());
  n = atoi(arr[4].c_str());
  h = atoi(arr[3].c_str());
  w = day_clock::local_day().day_of_week().as_number();
  d = atoi(arr[2].c_str());
  m = atoi(arr[1].c_str());
  y = day_clock::local_day().year();
}

void date2(int& y, int&m, int& d, int& w, int& h, int& n, int& s)
{
  date d_ = day_clock::local_day();
  time_duration now = second_clock::local_time().time_of_day();

  y = d_.year();
  m = d_.month();
  d = d_.day();
  w = d_.day_of_week().as_number();

  h = now.hours();
  n = now.minutes();
  s = now.seconds();
}

int main()
{
  using namespace boost::lambda;
  int const iter = 1000;

  int y, m, d, w, h, mm, s;

  timer t0;
  for (int i=0; i<iter; i++)
  {
    date1(y, m, d, w, h, mm, s);
  }

  cout << t0.elapsed() << endl;
  cout << str(format("%d %d %d %d %d %d %d\n") %
              y % m % d % w % h % mm % s);

  timer t1;
  for (int i=0; i<iter; i++)
  {
    date2(y, m, d, w, h, mm, s);
  }

  cout << t1.elapsed() << endl;;
  cout << str(format("%d %d %d %d %d %d %d\n") %
              y % m % d % w % h % mm % s);
}
