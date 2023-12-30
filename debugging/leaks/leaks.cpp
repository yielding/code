#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  int* ptr = nullptr;
  for (int i=0; i<10; i++)
  {
    ptr = new int;
  }
  
  return 0;
}