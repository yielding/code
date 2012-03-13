#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>
#include "active_regex.h"

using namespace std;

int main(int argc, char const* argv[])
{
  // 1. file prepare
  std::fstream in;
  in.open("test_1g.bin", ios_base::binary | ios_base::in);

  boost::regex e("\xff\xff");

  ActiveRegex regex;

  regex.buffer_size(1024*4);

  if (regex.search(e, in, false))
  {
    matches const& r = regex.result();

    cout <<  r[1].offset;
  }

  return 0;
}
