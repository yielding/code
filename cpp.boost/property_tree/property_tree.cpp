#include <boost/property_tree/json_parser.hpp>

#include <iostream>
#include <vector>

using namespace std;

namespace pt = boost::property_tree;

int main(int argc, char *argv[])
{
  pt::ptree tree;
  pt::read_json("./libraries.json", tree);

  // 1. read primitive data
  cout << tree.get<string>("name") << endl;

  // 2. read array
  for (auto author : tree.get_child("authors"))
    cout << author.second.data() << endl;

  return 0;
}
