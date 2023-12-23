#include <iostream>
#include <vector>
#include <string>
#include <fstream>

#include <range/v3/view/transform.hpp>
#include <range/v3/range/conversion.hpp>

using namespace ranges::v3;
using namespace std;

auto count_lines(const string& filename) -> int
{ 
  ifstream in(filename);

  return in.good() 
    ? count(istreambuf_iterator<char>(in), 
            istreambuf_iterator<char>(), '\n')
    : 0;
}

auto count_lines_in_files(vector<string>& files) -> vector<int> 
{ 
  return files 
    | view::transform(count_lines) 
    | to<vector>();
}

int main(int argc, char* argv[])
{
  vector<string> files {
    "/Users/yielding/code/big.read.write.cpp",
    "/Users/yielding/code/big.read.write.cpp"
  };

  auto res = count_lines_in_files(files);
  for (auto r : res)
    cout << r << " ";

  return 0;
}