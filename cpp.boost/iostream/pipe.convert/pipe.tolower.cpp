#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filter/counter.hpp>
#include <boost/iostreams/filtering_stream.hpp>

#include <iostream>

using namespace std;

namespace boost { namespace iostreams {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct tolower_filter : public input_filter
{
  template<typename Source>
  bool get(Source& s)
  {
    int c = boost::iostreams::get(s);
    return c != EOF && c != WOULD_BLOCK ? std::tolower(uint8_t(c)) : c;
  }
};

BOOST_IOSTREAMS_PIPABLE(tolower_filter, 0)

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct toupper_multichar_filter : public multichar_input_filter 
{
  template<typename Source>
  std::streamsize read(Source& s, char* buf, std::streamsize n)
  {   
    std::streamsize result = boost::iostreams::read(s, buf, n); 
    if (result == -1) 
      return -1; 

    for (int z = 0; z < result; ++z)
      buf[z] = (char) std::toupper((unsigned char) buf[z]);

    return result;
  }   
};

BOOST_IOSTREAMS_PIPABLE(toupper_multichar_filter, 0)
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
  
namespace io = boost::iostreams;

int main(int argc, char const* argv[])
{
  io::filtering_istream in;
  in.push(io::toupper_multichar_filter());
  in.push(io::file_source("hello"));

  string s;
  getline(in, s); cout << s << endl;
  getline(in, s); cout << s << endl;
  getline(in, s); cout << s << endl;

  return 0;
}
