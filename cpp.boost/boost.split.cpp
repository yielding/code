#include <string>
#include <iostream>
#include <vector>
#include "boost/algorithm/string.hpp"
  
using namespace std;      

int main()
{
  string a = "leech:kamin";
  vector<string>v;
  boost::split(v, a, boost::is_any_of(":"));
  
  cout << v[0] << " : " << v[1] << endl;
}
