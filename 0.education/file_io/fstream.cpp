#include <fstream>
#include <iostream>
#include <cstdint>

using namespace std;

int main(int argc, char const* argv[])
{
  fstream io;
  io.open("test.bin", ios::binary | ios::in | ios::out | ios::trunc);
  if (!io.is_open())
    return 1;

  char buffer[] = { 'l', 'e', 'e', 'c', 'h' };

  io.write((char*)buffer, 5);
  io.seekp(0, ios::beg);
  char const* d = "k";
  io.write(d, 1);

  char data[10] = { 0 };
  io.seekg(3);
  io.read(data, 2);
  cout << io.gcount() << endl;
  cout << data;

  return 0;
}
