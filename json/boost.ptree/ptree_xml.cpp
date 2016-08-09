#include <boost/property_tree/xml_parser.hpp>
#include <boost/algorithm/string.hpp>

#include <sstream>
#include <iostream>
#include <cassert>

using namespace std;
      namespace pt = boost::property_tree;

int main(int argc, char *argv[])
{
  stringstream ss;
  ss << "<MD-NEXT><Path> c:/Users/yielding/Desktop\r\n</Path></MD-NEXT>";

  pt::ptree tree;
  pt::read_xml(ss, tree);

  auto r0 = tree.get<string>("MD-NEXT.Path", "");
  boost::trim(r0);
  assert(r0 == "c:/Users/yielding/Desktop"s);

  auto r1 = tree.get<string>("MD-NEXT.Path2", "error");
  boost::trim(r1);
  assert(r1 == "error"s);

  return 0;
}
