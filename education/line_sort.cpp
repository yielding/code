#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

int main(int argc, const char *argv[])
{
  assert(argc == 2);

  ifstream ifs(argv[1]);

  vector<string> lines;
  string line;
  while (getline(ifs, line)) 
    lines.push_back(line);

  sort(lines.begin(), lines.end(), [](auto& s1, auto& s2) {
    return s1.length() < s2.length();
  });
  
  for (auto line: lines) cout << line << endl;

  return 0;
}
