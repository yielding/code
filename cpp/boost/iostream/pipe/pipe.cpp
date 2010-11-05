#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/counter.hpp>
#include <boost/iostreams/tee.hpp>
#include <boost/ref.hpp>

#include <string>
#include <iostream>

namespace io = boost::iostreams;

int main()
{
  BOOST_IOS::openmode const out_mode 
    = BOOST_IOS::app | BOOST_IOS::binary;

  io::counter c;
  io::filtering_ostream out;

  out.push(boost::ref(c));
  out.push(io::tee(io::file_sink("hello.tee", out_mode)));
  out.push(io::file_sink("hello"));

  out << "leech ";
  out << "kamin ";
  out << "gunhee\n";
  out.flush();

  std::cout << c.lines() << std::endl;
  std::cout << c.characters();
}
