#include <fstream>
#include <iostream>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/bzip2.hpp>

namespace io = boost::iostreams;

int main(int argc, char const* argv[])
{
  using namespace std;

  ifstream file("hello.bz2", ios_base::in | ios_base::binary);
  io::filtering_streambuf<io::input> in;

  in.push(io::bzip2_decompressor());
  in.push(file);

  io::copy(in, cout);

  return 0;
}

