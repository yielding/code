#include <cassert>
#include <string>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/detail/ios.hpp> // ios_base::beg.
#include "container_device.hpp"

namespace io = boost::iostreams;
namespace ex = boost::iostreams::example;

int main()
{
    using namespace std;
    typedef ex::container_device<string> string_device;

    string one, two;
    io::stream<string_device>  io(one);
    io << "Hello World!\n";
    io.flush();
    io.seekg(0, ios_base::beg);
    getline(io, two);

    assert(two == "Hello World!");

    io.seekg(0, ios_base::beg);
    assert(io.tellg() == 0);
    getline(io, two);
    assert(two == "Hello World!");

    getline(io, two);
    assert(two == "");
    io.clear();
    io.seekg(0, ios_base::beg);
    cout << io.tellg() << endl;
    assert(io.tellg() == 0);
}
