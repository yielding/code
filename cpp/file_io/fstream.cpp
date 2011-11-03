#include <fstream>
#include <iostream>
#include <stdint.h>

using namespace std;

int main(int argc, char const* argv[])
{
  fstream io;
  io.open("test.bin", ios_base::binary|ios_base::in|ios_base::out|ios_base::trunc);
  if (!io.is_open())
    return 1;

  uint8_t buffer[] = { 'l', 'e', 'e', 'c', 'h' };

  io.write((char*)buffer, 5);
  io.seekp(0, ios_base::beg);
  char* d = "k";
  io.write(d, 1);

  char data[10] = { 0 };
  io.seekg(3);
  io.read(data, 2);
  cout << io.gcount() << endl;
  cout << data;

  return 0;
}
