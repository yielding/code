#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/iostreams/stream.hpp>
#include <vector>

namespace io = boost::iostreams;

int main() 
{
  using namespace std;
  vector<string> strArray(2000000);

  io::mapped_file_params params;
  params.path          = "text.txt";
  params.new_file_size = 30ul << 30;
  params.flags         = io::mapped_file::mapmode::readwrite;

  io::stream<io::mapped_file_sink> out(params);

  copy(strArray.begin(), strArray.end(), ostream_iterator<string>(out, "\n"));

  return 0;
}
