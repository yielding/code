#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/device/file.hpp>

namespace io = boost::iostreams;

int main(int argc, char const* argv[])
{
  io::filtering_ostream out;

  out.push(io::file_sink("leech.txt"));
  out << "leech";

  return 0;
}
