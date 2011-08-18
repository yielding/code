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
struct Manifest
{
  bool was_passcode_set;
  bool is_encrypted;
  string product_version;
  string product_type;
  vector<string> apps;

  string to_s()
  {
    using namespace boost;
    auto passcode  = was_passcode_set ? "pass on" : "pass off";
    auto encrypted = is_encrypted ? "encrypted" : "not encrypted";
    auto settings  = str(format("%s\n%s\n%s\n%s\n") 
                   % passcode % encrypted % product_version % product_type);
    auto apps_     = join_(apps.begin(), apps.end());
    return settings + "[" + apps_+ "]";
  } 
};

int main()
{
  utility::parser::PTreeParser ptree("Manifest.plist");
  if (!ptree.init())
    return 0;

  string key1 = "plist.dict";
  string key2 = "plist.dict.dict";
  string filter = "key, string, true, false, date";
  auto l1 = ptree.enumerate(key1, 0, filter)
                 .enumerate(key2, 0, filter).pairs();

  Manifest m;
  for (auto it = l1.begin(); it !=l1.end(); ++it)
  {
    auto const& key = it->first; 
    if (key == "IsEncrypted")
      m.is_encrypted = it->second == "true";
    else if (key == "WasPasscodeSet")
      m.was_passcode_set = it->second == "true";
    else if (key == "ProductVersion")
      m.product_version = it->second;
    else if (key == "ProductType")
      m.product_type = it->second;
    else 
      continue;
  }

  ptree.clear()
    .enumerate(key2, 1, "key, string, dict")
    .filter("first")
    .reduce_to(m.apps);

  cout << m.to_s();

  return 0;
}
