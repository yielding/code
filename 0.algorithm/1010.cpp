#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>

using namespace std;

struct Line
{
  Line(string& w, string& e)
  {
    west = stoi(w);
    east = stoi(e);
  }

  int west;
  int east;
};

auto split(string line, char delim=' ') -> vector<string> 
{
  vector<string> result;
  istringstream ss{line};

  string res;
  while (getline(ss, res, delim))
  {
    if (res.size() <= 0) continue;
    result.push_back(res);
  }

  return result;
}

auto read_input_file(ifstream& ifs) -> vector<Line> 
{
  vector<Line> result;

  string input; getline(ifs, input);
  int count = stoi(input);
  for (int i=0; i<count; i++)
  {
    string l; getline(ifs, l);
    auto nk = split(l);
    assert(nk.size() == 2);

    result.emplace_back(nk[0], nk[1]);
  }

  return result;
}

long combination(int n, int k)
{
  assert(n >= k);

  if (n == k || k == 0)
    return 1;

  return combination(n-1, k-1) + combination(n-1, k);
}

int main(int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: 1010 ./input.txt" << endl;
    return 1;
  }

  ifstream ifs{argv[1]};
  if (!ifs.good())
  {
    cerr << "bad";
    return 1;
  }

  auto res = read_input_file(ifs);

  for (auto& line: res)
  {
    cout << "west: " << line.west << ", ";
    cout << "east: " << line.east << endl;

    cout << combination(line.east, line.west) << endl;
  }


  return 0;
}