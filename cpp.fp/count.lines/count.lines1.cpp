#include <iostream>
#include <vector>
#include <string>
#include <fstream>

using namespace std;

auto count_lines_in_files(const vector<string>& files) -> vector<int> 
{ 
  vector<int> results(files.size());

  for (auto& file : files)
  {
    ifstream in(file);
    auto c = in.good() 
      ? count(istreambuf_iterator<char>(in), 
              istreambuf_iterator<char>(), 
              '\n') 
      : 0;

    results.push_back(c);
  }

  return results;
}

int main(int argc, char* argv[])
{
  const auto files = vector{
    "/Users/yielding/code/big.read.write.cpp"s,
    "/Users/yielding/code/big.read.write.cpp"s
  };

  auto res = count_lines_in_files(files);

  for (auto r : res)
    cout << r << " ";

  return 0;
}