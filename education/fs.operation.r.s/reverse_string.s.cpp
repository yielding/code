#include <string>
#include <stack>
#include <fstream>
#include <iostream>

using namespace std;

int main(int argc, const char *argv[])
{
  ifstream ifs("data.txt");

  stack<string> strs;
  string line;
  while(getline(ifs, line))
    strs.push(line);

  while (!strs.empty())
  {
    cout << strs.top() << endl;
    strs.pop();
  }

  return 0;
}
