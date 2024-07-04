#include <print>
#include <format>
#include <string>
#include <cstdlib>
#include <fstream>
#include <numeric>

#include <boost/uuid/detail/sha1.hpp>
#include <filesystem>

using namespace std; 
      namespace fs = filesystem;

int main(int argc, char *argv[])
{
  string fname = argc == 1 ? "zero.bin" : argv[1];

  ifstream in(fname, ios_base::binary);
  if (!in.is_open())
    return EXIT_FAILURE;

  auto size = fs::file_size(fname);

  auto buffer = new char[size];

  in.read(buffer, size);

  boost::uuids::detail::sha1 sh;
  sh.process_bytes(buffer, size);

  unsigned int digest[5];
  sh.get_digest(digest);

  string r;
  for (auto i : digest) r += format("{0:x}", i);

  println("{}", r);

  delete [] buffer;
  
  return EXIT_SUCCESS;
}