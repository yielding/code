#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <exception>
#include <iostream>

using namespace std;
using boost::property_tree::ptree;

void print_all(ptree& pt)
{
}

void print_selected(ptree& pt)
{
    ptree::iterator beg = pt.get_child("plist.dict.array.dict").begin();
    ptree::iterator end = pt.get_child("plist.dict.array.dict").end();

    for (int i=0; beg != end; ++beg, i++)
    {
        if (i == 8)
            break;

        cout << beg->second.data() << endl;
    }
}

int main()
{
    try
    {
        ptree pt;
        read_xml("history.xml", pt);
        print_selected(pt);

    }
    catch (std::exception &e)
    {
        std::cout << "error: " << e.what() << "\n";
    }

    return 0;
}
