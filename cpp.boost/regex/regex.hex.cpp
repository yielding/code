#include <cstdlib>
#include <string>
#include <iostream>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;

int main()
{
  // string text = "+xrC5rLKU- +u/zIcQ- +yRHXZcdY- +xe3ArMgB- +wKy6hcdE- +t0Ss4A- +x3S1RcXQ- +0NzFtLCssuQ-.abcdefghijklmnopqrstuvwxyz";
                // L"우리는 민족 중흥의 역사적 사명을 띄고 이땅에 태어났다.abcdefghijklmnopqrstuvwxyz"
  string text = "leech kamin";
  boost::regex re("^leech.+");
  boost::smatch what;
  if (boost::regex_match(text, what, re))
  {
    // what[0] contains the whole string
    // what[1] contains the response code
    // what[2] contains the separator character
    // what[3] contains the text message.

    cout << "ok";
  }
  else
  {
    cout << "not ok";
  }

  return 0;
}
