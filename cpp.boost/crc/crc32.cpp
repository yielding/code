#include <iostream>
#include <fstream>
#include <string>

#include <boost/crc.hpp>

using namespace std;

int get_crc32(string const& s)
{
  boost::crc_32_type result;
  result.process_bytes(s.data(), s.length());
  return result.checksum();
}

int main(int argc, const char *argv[])
{
  boost::crc_32_type result;
  ifstream ifs("./crc32.cpp", ios_base::binary);

  int const BUF_SIZE = 1024;

  if (ifs) do
  {
    char buffer[BUF_SIZE] = { 0 };
    ifs.read(buffer, BUF_SIZE);
    result.process_bytes(buffer, ifs.gcount());
  }
  while (ifs);

  cout << "res: " << hex << result.checksum();

  return 0;
}
