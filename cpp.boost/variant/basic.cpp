#include <boost/variant.hpp>

#include <string>
#include <iostream>

using namespace boost;
using namespace std;

int main(int argc, char *argv[])
{
  variant<int, double> v;
  v = 12.34;
  cout << get<double>(v) << endl;
  
  v = 12;
  cout << get<double>(v) << endl;
  return 0;
}
