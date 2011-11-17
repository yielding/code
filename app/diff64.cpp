#include <boost/filesystem/operations.hpp>
#include <stdint.h>
#include <iostream>
#include <fstream>

using namespace std;
using namespace boost::filesystem;

int main(int argc, char const* argv[])
{
  if (argc != 3)
  {
    cout << "usage: diff64 first_file second_file\n";
    return 1;
  }

  ifstream f0;
  f0.open(argv[1]);
  if (!f0.is_open()) 
    return 1;

  ifstream f1;
  f1.open(argv[2]);
  if (!f1.is_open()) 
    return 1;

  int64_t fs0 = file_size(argv[1]);
  int64_t fs1 = file_size(argv[2]);

  if (fs0 != fs1)
  {
    cout << "differend...\n";
    return 1;
  }

  cout << "file size: " << fs0 << endl;
  cout.flush();

  int const block_size = 8*1024;

  uint8_t b0[block_size] = { 0 };
  uint8_t b1[block_size] = { 0 };

  int64_t left = fs1;
  while (left > 0)
  {
    int64_t to_read = (left > block_size) ? block_size : left;

    f0.read((char*)b0, to_read);
    f1.read((char*)b1, to_read);

    for (int i=0; i<to_read; i++)
    {
      if (b0[i] != b1[i])
      {
        cout << "differend...\n";
        return 1;
      }
    }

    left -= to_read;
  }

  cout << "ok.. same\n";

  return 0;
}
