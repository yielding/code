#include <iostream>
#include <vector>
#include <string>
#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/exceptions.hpp>
#include <boost/property_tree/json_parser.hpp>

using namespace std;

int main(int argc, char const* argv[])
{
  using boost::property_tree::ptree;
  typedef vector<string> strings;

  try
  {
    ptree pt;
    read_json("/Users/yielding/code/app/motoroi/picture.json", pt);
    cout << "total block: " << pt.get<std::string>("TotalBlocks") << endl;
    cout << "total count: " << pt.get<std::string>("TotalCount") << endl;
    cout << "block index: " << pt.get<std::string>("BlockIndex") << endl;
    cout << "block size : " << pt.get<std::string>("BlockSize") << endl;
    cout << "count      : " << pt.get<std::string>("Count") << endl;

    cout << "resp. count: " <<pt.get_child("RESPONSE").size() << endl;

    BOOST_FOREACH(auto &v, pt.get_child("RESPONSE"))
    {
      ptree const& t = v.second;
      cout << "( " 
           << t.get<std::string>("id") << " " 
           << t.get<std::string>("PicName") << " "
           << t.get<std::string>("LOC") << " "
           << t.get<std::string>("TLOC") << " "
           << t.get<std::string>("Size") << " "
           << t.get<std::string>("LastModified") << " "
           << t.get<std::string>("AlbumName") << " "
           << t.get<std::string>("Orientation") << " "
           << t.get<std::string>("Width") << " "
           << t.get<std::string>("Height") << " )\n";
    }
  }
  catch(boost::property_tree::ptree_bad_data& e)
  {
    cout << "exception: " << e.what();
  }
  catch(std::exception& e)
  {
    cout << "exception: " << e.what();
  }

  return 0;
}
