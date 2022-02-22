#include <iostream>
#include <vector>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

using namespace std;
using namespace boost::property_tree;

int main(void)
{
  ptree pt;
  read_xml("data.xml", pt);

  if (auto str = pt.get_optional<string>("root.str"))
    cout << str.get() << endl;

  for (auto child: pt.get_child("root.values"))
  {
    auto v = child.second.data();
    cout << v << endl;
  }

  return 0;
}
