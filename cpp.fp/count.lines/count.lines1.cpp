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

  if (!in.good())
    return 0;

  return count(
    istreambuf_iterator<char>(in), 
    istreambuf_iterator<char>(), 
    '\n'
  );
}

auto count_lines_in_files(const vector<string>& files) -> vector<int> 
{ 
  vector<int> results(files.size());

  transform(files.cbegin(), files.cend(), 
            results.begin(), 
            count_lines);

  return results;
}

int main(int argc, char* argv[])
{
  vector<string> files{
    "/Users/yielding/code/big.read.write.cpp",
    "/Users/yielding/code/big.read.write.cpp"
  };

  auto res = count_lines_in_files(files);

  for (auto r : res)
    cout << r << " ";

  return 0;
}