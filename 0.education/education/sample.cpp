#include <fstream>
#include <iostream>
#include <string>
#include <algorithm>
#include <cassert>
#include <map>

using namespace std;

int main(int argc, const char *argv[])
{
  assert(argc == 2);
  ifstream ifs(argv[1]);

  map<char, unsigned> sample;
  auto sampler = [&sample](char ch) { sample[tolower(ch)]++; };

  string line;
  for (int no=0; getline(ifs, line); ++ no)
    for_each(line.begin(), line.end(), sampler);

  for (auto it=sample.begin(); it!=sample.end(); ++it)
  {
    auto ch = it->first;
    auto count = it->second;
    if (isalpha(ch))
      cout << ch << " : " << count << endl;
  }
  
  return 0;
}
