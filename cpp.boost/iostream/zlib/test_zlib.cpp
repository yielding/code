#include <fstream>
#include <iostream>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/zlib.hpp>

int main(int argc, char const* argv[])
{
  using namespace std;
  namespace io = boost::iostreams;

  ifstream file("hello.gz", ios_base::in | ios_base::binary);

  io::filtering_streambuf<io::input> in;
  in.push(io::zlib_decompressor());
  in.push(file);

  io::copy(in, cout);
  
  return 0;
}
