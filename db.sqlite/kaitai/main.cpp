#include <iostream>
#include <string>
#include <fstream>
#include "kaitai/kaitaistream.h"

using namespace std;

int main(int argc, char* argv[])
{
  string path = "";
  ifstream is(path, ios::binary);

  kaitai::kstream ks(&is);


  return 0;
}