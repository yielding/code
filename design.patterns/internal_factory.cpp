#include <iostream>
#include <string>
#include <cmath>

using namespace std;

class Point
{
private:
  struct PointFactory
  {
    PointFactory() = delete;

  public:
    static Point NewCartesian(float x, float y)
    {
      return {x, y};
    }

    static Point NewPolar(float r, float theta)
    {
      return { r * cos(theta), r * sin(theta) };
    }
  };

public:
  float x;
  float y;
  static PointFactory Factory;

  auto to_s() -> string
  {
    return "{ "s + to_string(x) + ", " + to_string(y) + " }";
  }
};

int main(int argc, char *argv[])
{
  auto p0 = Point::Factory.NewPolar(10, 20);
  auto p1 = Point::Factory.NewCartesian(10, 20);

  cout << p0.to_s() << endl;
  cout << p1.to_s() << endl;
  
  return 0;
}
