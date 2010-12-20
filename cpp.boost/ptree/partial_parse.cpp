#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <exception>
#include <iostream>

using namespace std;

int main()
{
  try
  {
    using boost::property_tree::ptree;
    ptree pt;

    read_xml("History.xml", pt);

    ptree::iterator beg = pt.get_child("plist.dict.array.dict").begin();
    ptree::iterator end = pt.get_child("plist.dict.array.dict").end();
    for (int i=0; beg != end; ++beg, i++)
    {
      if (i == 8)
        break;

      cout << beg->second.data() << endl;
    }
  }
  catch (std::exception &e)
  {
    std::cout << "Error: " << e.what() << "\n";
  }

  return 0;
}
