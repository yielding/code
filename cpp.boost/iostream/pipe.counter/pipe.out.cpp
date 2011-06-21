#include <boost/iostreams/detail/ios.hpp> // ios_base::beg.
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/counter.hpp>
#include <boost/iostreams/tee.hpp>
#include <boost/ref.hpp>

#include <string>
#include <iostream>

using namespace std;
      namespace io = boost::iostreams;

int main()
{
    ios_base::openmode const out_mode 
        = ios_base::app | ios_base::binary;

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
