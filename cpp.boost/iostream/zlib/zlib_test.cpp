#include <fstream>
#include <iostream>
#include <boost/iostream/filtering_streambuf.hpp>
#include <boost/iostream/copy.hpp>
#include <boost/iostream/zlilb.hpp>

int main(int argc, char const* argv[])
{
  using namespace std;
  namespace io = boost::iostreams;

  ifstream file("hello.z", ios_base::binary);

  io::filtering_streambuf<io::input> in;
  in.push(io::zlib_decompressor());
  in.push(file);

  io::copy(in, cout);
  
  return 0;
}
