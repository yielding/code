#include <iostream>

using namespace std;

float inv_sqrt(float x)
{
  float xhalf = 0.5f*x;
  int i = *(int *)&x;

  i = 0x5f3759df - (i >> 1);
  x = *(float *)&i;
  x = x * (1.5f - xhalf*x*x);

  return x;
}

int main (int argc, char const* argv[])
{
  cout << inv_sqrt(4);

  return 0;
}
