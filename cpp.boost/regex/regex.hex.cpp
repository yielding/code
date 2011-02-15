#include <cstdlib>
#include <string>
#include <iostream>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;

bool test_binary_smatch()
{
  string text = "leech kamin leeks";
  text[0] = 0xFD;
  boost::regex re("\xFD\x65\x65\x63\x68");
  boost::smatch what;
  if (boost::regex_search(text, what, re))
  {
    // what[0] contains the whole string
    // what[1] contains the response code
    // what[2] contains the separator character
    // what[3] contains the text message.

    cout << "ok.";
#ifdef VERBOSE
    cout << what[0] << endl;
#endif
    return true;
  }
  else
  {
    // cout << "not ok.";
    return false;
  }
}

int main()
{
  assert(test_binary_smatch());

  return 0;
}
