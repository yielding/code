#include <iostream>
#include <fstream>
#include <string>

using namespace std;

string const file="test.bin";

void create(ofstream& ofs)
{
  char buffer[] = { 'l', 'e', 'e', 'c', 'h', 1, 2, 3};
  ofs.open(file.c_str(), ios::out|ios::binary);

  ofs.write(buffer, sizeof buffer / sizeof buffer[0]);
  ofs.close();
}

void append(ofstream& ofs)
{
  char buffer[] = { 4, 5, 6 };
  ofs.open(file.c_str(), ios::out|ios::binary|ios::app);

  ofs.write(buffer, sizeof(buffer) / sizeof(buffer[0]));
  ofs.close();
}

bool read_and_test()
{
  ifstream ifs;
  ifs.open(file.c_str(), ios::in);
}

int main(int argc, char const* argv[])
{
  ofstream ofs;

  create(ofs);
  append(ofs);
  read_and_test();
  
  return 0;
}
