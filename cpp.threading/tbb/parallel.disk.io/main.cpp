#include <iostream>
#include <fstream>
#include <vector>
#include <tbb/parallel_for.h>
#include <tbb/blocked_range.h>

using namespace std;

const size_t BLOCK_SIZE = 1024 * 1024; // 1MB block size

void read_blocks(const vector<string>& files) 
{
  vector<char> buffer(BLOCK_SIZE);
  tbb::parallel_for(tbb::blocked_range<size_t>(0, files.size()), 
    [&](const tbb::blocked_range<size_t>& r) {
      for (size_t i = r.begin(); i < r.end(); i++) 
      {
        ifstream file(files[i], ios::binary);
        while (file) 
        {
          file.read(buffer.data(), buffer.size());
          // process the data in buffer
          // ...
        }
      }
  });
}

int main() 
{
    vector<string> files = {"file1.bin", "file2.bin", "file3.bin"};
    read_blocks(files);

    return 0;
}
