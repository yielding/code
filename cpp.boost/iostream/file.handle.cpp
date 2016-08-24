#include <stdio.h>
#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;

int main()
{
  FILE* file = fopen("/tmp/file.txt", "w");
  int fd = fileno(file);
  io::file_descriptor_sink fdsink(fd);
  io::stream<io::file_descriptor_sink> out(fdsink);

  out << "FILE* " << file << " fd: " << fd << "\n";

  return 0;
}
