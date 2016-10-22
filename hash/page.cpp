#include <iostream>
#include <string>
#include <cstdlib>

#include <boost/uuid/sha1.hpp>
#include <boost/format.hpp>
#include <boost/filesystem.hpp>

using namespace std; 
using namespace boost;
      namespace fs = boost::filesystem;

int main(int argc, char *argv[])
{
  string fname = argc == 1 ? "zero.bin" : argv[1];

  ifstream in(fname, ios_base::binary);
  if (!in.is_open())
    return EXIT_FAILURE;

  auto size = fs::file_size(fname);

  auto buffer = new char[size];

  in.read(buffer, size);

  uuids::detail::sha1 sh;
  sh.process_bytes(buffer, size);

  unsigned int digest[5];
  sh.get_digest(digest);

  string r;
  for (auto i : digest)
    r += str(format("%08x") % i);

  cout << r << endl;
  

  return EXIT_SUCCESS;
}
