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

  auto comp = [](string const& s1, string const& s2) {
    return s1.length() < s2.length();
  };

  sort(lines.begin(), lines.end(), comp);
  
  for (auto it=lines.begin(); it!=lines.end(); ++it)
      cout << *it << endl;

  return 0;
}
