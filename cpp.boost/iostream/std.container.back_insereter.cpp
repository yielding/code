#include <cassert>
#include <string>
#include <boost/iostreams/device/back_inserter.hpp>
#include <boost/iostreams/filtering_stream.hpp>

namespace io = boost::iostreams;

int main()
{
  using namespace std;

  string                 result;
  io::filtering_ostream  out(io::back_inserter(result));
  out << "Hello World!";
  out.flush();
  assert(result == "Hello World!");
}
