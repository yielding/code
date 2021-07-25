#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

using strings = vector<string>;

auto sort_lines_with_selection_sort(strings lines) -> strings
{
  for (auto i=0; i<lines.size()-1; i++)
  {
    for (auto j=i+1; j<lines.size(); j++)
    {
      if (lines[i].length() > lines[j].length())
      {
        auto temp = lines[j];
        lines[j] = lines[i];
        lines[i] = temp;
      }
    }
  }

  return lines;
}

auto sort_lines_with_api(strings lines) -> strings 
{
  auto comp = [](auto& s1, auto& s2) { return s1.length() < s2.length(); };

  sort(lines.begin(), lines.end(), comp);

  return lines;
}

void print_lines(const strings& lines)
{
  for (auto& line: lines)
    cout << line << endl;
}

auto read_lines(const string& path) -> strings
{
  ifstream ifs(path);

  strings lines;
  string line;

  while (getline(ifs, line)) lines.push_back(line);

  return lines;
}

int main(int argc, const char *argv[])
{
  assert(argc == 2);

  auto lines = read_lines(string(argv[1]));

  auto r0 = sort_lines_with_selection_sort(lines);
  print_lines(r0);

  auto r1 = sort_lines_with_api(lines);
  print_lines(r1);

  return 0;
}
