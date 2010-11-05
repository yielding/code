#include <fstream>
#include <iostream>

using namespace std;

int main()
{
  fstream in;
  in.open("aaa.txt", ios_base::binary | ios_base::in);

  in.seekg(0, ios_base::beg);
  ifstream::pos_type begin_pos = in.tellg();
  in.seekg(0, ios_base::end);

  cout << in.tellg() - begin_pos;
}
