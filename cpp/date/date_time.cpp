#include "boost/date_time/posix_time/posix_time.hpp"
#include <iostream>

using namespace std;

int main()
{
  using namespace boost::posix_time;
  using namespace boost::gregorian;

  // get the current time from the clock -- one second resolution
  ptime now = second_clock::local_time();
  cout << to_iso_extended_string(now) << endl;

  // Get the date part out of the time
  date today = now.date();
  date tommorrow = today + days(1);
  ptime tommorrow_start(tommorrow); //midnight

  // iterator adds by one hour
  time_iterator titr(now, hours(1));
  for (; titr<tommorrow_start; ++titr)
    std::cout << to_iso_extended_string(*titr) << std::endl;

  time_duration remaining = tommorrow_start - now;
  std::cout << "Time left till midnight: "
            << to_iso_string(remaining) << std::endl;

  return 0;
}
