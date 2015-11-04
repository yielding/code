#include <fstream>
#include <iostream>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>

int main(int argc, char const* argv[])
{
  using namespace std;
  namespace io = boost::iostreams;

  ifstream file("/tmp/zlib.cpp.gz", ios_base::in | ios_base::binary);

  io::filtering_streambuf<io::input> in;
  in.push(io::gzip_decompressor());
  in.push(file);

  io::copy(in, cout);
  
  return 0;
}
