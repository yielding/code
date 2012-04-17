#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>

#include "active_regex.h"

using namespace std;

int main(int argc, char const* argv[])
{
  string image_file = (argc < 2) 
    ? "/Users/yielding/Desktop/EV-F200_JTAG3_20120410.mdf"
    : argv[1]; 

  cout << image_file << endl;


  ifstream ifs;
  ifs.open(image_file.c_str(), ios_base::binary);
  if (!ifs.is_open())
    return EXIT_FAILURE;

  auto const& pattern = "(01[016789]{1}|02|0[3-9]{1}[0-9]{1})-?[0-9]{3,4}-?[0-9]{4}";
  boost::regex e(pattern);

  ActiveRegex regex;

  regex.buffer_size(1024*4);

  if (regex.search(e, ifs, false))
  {
    matches const& r = regex.result();

    for (auto it=r.begin(); it != r.end(); ++it)
      cout << it->offset << endl;

  }

  return EXIT_SUCCESS;
}
