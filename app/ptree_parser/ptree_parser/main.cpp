#include "PTreeParser.h"

#include <boost/format.hpp>
#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  template <typename ForwardIterator> 
  string join_(ForwardIterator first, ForwardIterator last, string const& sep=", ")
  {
    stringstream ss;
    ss << *first++;
    for (; first != last; ++first) ss << sep << *first;

    return ss.str();
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  utility::parser::PTreeParser ptree("/Volumes/Share.Disk/forensic.data/f8756d31fd023880.plist");
  if (!ptree.init_with("plist.dict"))
    return 0;

  auto r = ptree.get_dict_with("classKeys");
  for (auto it=r.begin(); it != r.end(); ++it)
  {
    cout << it->first << " : " << it->second << endl;
    cout.flush();
  }
  
  cout << "EMF     : " << ptree.get_string_with("EMF") << endl;
  cout << "Offset  : " << ptree.get_int_with("dataVolumeOffset") << endl;
  cout << "UUID    : " << ptree.get_string_with("dataVolumeUUID") << endl;
  cout << "hwModel : " << ptree.get_string_with("hwModel") << endl;
  cout << "key835  : " << ptree.get_string_with("key835") << endl;
  cout << "key89B  : " << ptree.get_string_with("key89B") << endl;
  
  return 0;
}
