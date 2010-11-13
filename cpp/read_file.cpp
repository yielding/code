#include <vector>
#include <iostream>
#include <fstream>

using namespace std;

vector<char> read_file(char const* name)
{
  ifstream file(name, ios_base::in|ios_base::binary);

  vector<char> buffer((istreambuf_iterator<char>(file)), 
                       istreambuf_iterator<char>());   

  return buffer;
}

int main(int argc, char const* argv[])
{
  cout << read_file(argv[1]).size();
  
  return 0;
}
