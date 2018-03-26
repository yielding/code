#include <iostream>
#include <new>

using namespace std;

struct Point
{
  Point() {}
  Point(int x, int y): m_x(x), m_y(y) {}
  int m_x, m_y;
};

union U
{
  int z;
  double w;
  Point p;       // <- possible now

  U()
  {
    new(&p) Point(); 
  }
};

int main(int argc, const char *argv[])
{
  U u;

  return 0;
}
