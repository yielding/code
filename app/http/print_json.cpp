#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <fstream>
#include <iostream>

using namespace std;
using namespace boost;

using boost::property_tree::ptree;

void print_leaf (ptree::iterator t, string space);
void print_array(ptree::iterator t, int index, string space);
void print_dict (ptree::iterator t, string space);
void print_tree (ptree& t, string space);

bool is_leaf(ptree::iterator i);
bool is_array_(ptree::iterator i);
bool is_dict(ptree::iterator i);

void print_leaf(ptree::iterator t, string space)
{
  space += "  ";

  // ptree::data_type data = t->second.data();
  string data = t->second.data();
  cout << space << t->first << " : " << data << endl;
}

void print_dict(ptree::iterator t, string space)
{
  space += "  ";
  cout << space << t->first << endl;

  int index_ = 0;
  for (auto i=t->second.begin(); i!=t->second.end(); ++i)
  {
    if (is_leaf(i))
      print_leaf(i, space);
    else if (is_array_(i))
      print_array(i, index_++, space);
    else if (is_dict(i))
      print_dict(i, space);
  }
}

void print_array(ptree::iterator t, int index, string space)
{
  space += "  ";
  cout << space << "item : " << index << endl;

  int index_ = 0;
  for (auto i=t->second.begin(); i!=t->second.end(); ++i)
  {
    if (is_leaf(i))
      print_leaf(i, space);
    else if (is_array_(i))
      print_array(i, index_++, space);
    else if (is_dict(i))
      print_dict(i, space);
  }
}

void print_tree(ptree& t)
{
  string space;
  int index_ = 0;

  for (auto i=t.begin(); i != t.end(); ++i)
  {
    if (is_leaf(i))
      print_leaf(i, space);

    if (is_array_(i))
      print_array(i, index_++, space);

    if (is_dict(i))
      print_dict(i, space);
  }
}

bool is_leaf(ptree::iterator i)
{
  return i->second.empty();
}

bool is_array_(ptree::iterator i)
{
  if (is_leaf(i))
    return false;

  auto first_child = i->second.begin();

  return first_child->first.empty();

  // return is_leaf(i) ? false : i->first.empty();
}

bool is_dict(ptree::iterator i)
{
  return is_leaf(i) ? false: !i->first.empty();
}

int main(int argc, char const* argv[])
{
  ifstream fi; fi.open("picture1.json");
  ptree pt; read_json(fi, pt);
  print_tree(pt);

  return 0;
}
