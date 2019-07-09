#include <cmath>
#include <iostream>
#include <string>

using namespace std;

class Point
{
public:
  static Point NewCartesian(float x, float y)
  {
    return { x, y };
  }

  static Point NewPolar(float r, float theta)
  {
    return { r * cos(theta), r * sin(theta) };
  }

  auto to_s() -> string
  {
    return "{ "s + to_string(x) + ", " + to_string(y) + " }";
  }

protected:
  Point(const float x, const float y)
    : x{x}, y{y}
  {}

private:
  float x, y;
};

int main(int argc, char *argv[])
{
  auto p0 = Point::NewCartesian(10, 20);
  auto p1 = Point::NewPolar(10, 45);

  cout << p0.to_s() << endl;
  cout << p1.to_s() << endl;
  
  return 0;
}
