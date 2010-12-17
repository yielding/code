#include <boost/xpressive/xpressive.hpp>

#include <iostream>

using namespace boost::xpressive;
using namespace std;

int main()
{
  std::string hello("hello world!");
  sregex rex = sregex::compile("(\\w+) (\\w+)!");
  smatch what;

  if (regex_match(hello, what, rex))
  {
    cout << what[0] << "\n";
    cout << what[1] << "\n";
    cout << what[2] << "\n";
  }
  else
  {
    cout << "no match\n";
  }

  return 0;
}
