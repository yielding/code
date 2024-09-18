#include <iostream>
#include <fstream>
#include <vector>
#include <string>

#include <range/v3/view/transform.hpp>
#include <range/v3/range/conversion.hpp>

using namespace ranges;
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
    | transform(count_lines)
    | to<vector>();
}

int main(int argc, char* argv[])
{
  auto files = vector{
    "/Users/yielding/code/big.read.write.cpp"s,
    "/Users/yielding/code/big.read.write.cpp"s
  };

  auto res = count_lines_in_files(files);
  for (auto r : res)
    cout << r << " ";

  return 0;
}