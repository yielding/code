#include <ostream>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>

namespace io = boost::iostreams;

int main(int argc, char const* argv[])
{
  io::stream_buffer<io::file_sink> buf("log.txt");
  std::ostream out(&buf);

  for (int i=0; i<10; i++)
    out << "leech\n";

  return 0;
}
