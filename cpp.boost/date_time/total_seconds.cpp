#include <iostream>
#include <sys/time.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

using namespace std;
using namespace boost::posix_time;
using namespace boost::gregorian;

int main(int argc, char *argv[])
{
  timeval t;

  for (;;)
  {
    timeval t;
    gettimeofday(&t, NULL);
    cout << t.tv_sec << endl;

    ptime beg(date(1970,Jan,1));
    ptime now(second_clock::local_time());

    time_duration diff = now - beg;
    cout << diff.total_seconds() << endl;

    sleep(1);
  }

  return 0;
}
