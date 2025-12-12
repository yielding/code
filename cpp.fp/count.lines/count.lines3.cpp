#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <range/v3/view/transform.hpp>
#include <range/v3/range/conversion.hpp>

using namespace ranges;
using namespace std;

auto open_file(string const& filename) -> ifstream 
{
  try { return ifstream{filename}; } catch (...) {}

  return ifstream{};
}

auto count_file_lines(ifstream in) -> int
{
  return static_cast <int>(count(
    istreambuf_iterator(in),
    istreambuf_iterator<char>(), '\n'));
}

auto count_lines_in_files2(vector<string>& files) -> vector<int> 
{ 
  return files 
    | views::transform(open_file)
    | views::transform(count_file_lines)
    | to<vector>();
}

int main()
{
  vector files = {
    "/Users/yielding/code/cpp.fp/count.lines/count.lines3.cpp"s,
    "/Users/yielding/code/cpp.fp/count.lines/count.lines3.cpp"s
  };

  vector v{1, 2};

  for (const auto res = count_lines_in_files2(files); const auto r : res)
    cout << r << " ";

  return 0;
}