#include <boost/regex.hpp>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;
using namespace boost;

int main()
{
  string in="|  |   |    | ||";
  
  regex e("  +");
  cout << regex_replace(in, e, " ");
}
