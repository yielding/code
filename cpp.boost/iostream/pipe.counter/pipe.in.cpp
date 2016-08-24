#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filter/counter.hpp>
#include <boost/iostreams/filtering_stream.hpp>

#include <iostream>

using namespace std;
      namespace io = boost::iostreams;

int main(int argc, char const* argv[])
{
  io::filtering_istream in;
  in.push(io::counter());
  in.push(io::file_source("hello"));

  string s;
  getline(in, s); getline(in, s); getline(in, s);
  cout << in.component<0, io::counter>()->lines() << endl;
  cout << in.component<0, io::counter>()->characters() << endl;

  return 0;
}
