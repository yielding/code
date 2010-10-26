#include <fstream>

using namespace std;

int main (int argc, char const* argv[])
{
  ofstream out;
  out.open("log.bin", ios_base::out|ios_base::binary|ios_base::trunc);
  for (int i=0; i<110; i++)
    out.write((char*)&i, sizeof(i));

  return 0;
}
