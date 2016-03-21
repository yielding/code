#include <iostream>
#include <string>
#include <vector>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace std;
using namespace boost::phoenix::arg_names;

int main(int argc, const char *argv[])
{
  vector<string> v {"one", "two", "three"};

  string two("two");
  two.append(1, 0);
  v.erase(remove_if(v.begin(), v.end(), arg1 == two), 
          v.end());

  for (auto e: v) cout << e << endl;
  for_each(v.begin(), v.end(), [](string& s) { s += "-1"; });
  for_each(v.begin(), v.end(), cout << arg1 << endl);
  for_each(begin(v), end(v), cout << arg1 << endl);

  return 0;
}
