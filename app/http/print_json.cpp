#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/foreach.hpp>
#include <sstream>
#include <fstream>
#include <iostream>

using namespace std;
using namespace boost;

using boost::property_tree::ptree;

void print_tree(ptree& t, string space)
{
  if (!t.empty())
    space += "  ";

  for (auto i=t.begin(); i !=t.end(); ++i)
  {
    cout << space << i->first << endl;
    if (!i->second.empty()) print_tree(i->second, space);
  }
}

int main(int argc, char const* argv[])
{
  ifstream fi;
  fi.open("picture.json");

  ptree pt; read_json(fi, pt);
  print_tree(pt, "  ");

  return 0;
}
