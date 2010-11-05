#include <string>
#include <iostream>

using namespace std;

int main()
{
  for (int i=0; i<20; i++)
  {
    char buf[10] = { 0 };
    sprintf(buf, "%02x ", i);
    cout << buf;
  }
}
