#include <fstream>
#include <iostream>
#include <string>

#include <boost/regex.hpp>

using namespace std;
using namespace boost;

regex phone_no("(01[016789]{1}|02|0[3-9]{1}[0-9]{1})-?[0-9]{3,4}-?[0-9]{4}");

bool process_phone_no(const char* page, string& res)
{
  cmatch what;
  if (regex_match(page, what, phone_no))
  {
    res = what[0];
    // what[0] contains the whole string
    // what[1] contains the response code
    // what[2] contains the separator character
    // what[3] contains the text message.
//    if (msg)
//      msg->assign(what[3].first, what[3].second);
//    return ::atoi(what[1].first);
    return true;
  }

  return false;
}

int main(int argc)
{
  ifstream ifs;
  ifs.open("/Users/yielding/code/cpp.boost/regex/data.txt");
  if (!ifs.is_open())
    return -1;

  string line;
  while (getline(ifs, line))
  {
    string res;
    if (process_phone_no(line.c_str(), res))
      cout << "matched: " << res << endl;
  }

  return 0;
}
