#include <string>
#include <fstream>
#include <iostream>
#include <boost/regex.hpp>
#include "parallel_regex.h"

using namespace std;

void serial_regex()
{
  std::fstream in; in.open("test_1m.bin", ios_base::binary | ios_base::in);

  PartialRegex searcher;
  searcher.buffer_size(1024*4);
  boost::regex e("\xff\xff");
  if (searcher.search(e, in))
  {
    matches const& r = searcher.result();
    cout <<  r[1].offset;
  }
}

int main(int argc, char const* argv[])
{

  return 0;
}
