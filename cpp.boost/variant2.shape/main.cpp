#include <boost/variant2/variant.hpp>
#include <vector>
#include <print>

using namespace std;
using namespace boost::variant2;

struct Recangle
{
  double area() const { return width * height; }

  int width;
  int height;
};

struct Circle
{
  double area() const { return 3.14 * radius * radius; }
  int radius;
};

double total_area(const vector<variant<Recangle, Circle>>& shapes)
{
  double total = 0;
  for (const auto& shape : shapes)
    total += visit([](auto const& s) { return s.area(); }, shape);

  return total;
}

int main(int argc, char* argv[])
{
  vector<variant<Recangle, Circle>> shapes;

  shapes.push_back(Recangle{5, 10});
  shapes.push_back(Circle{3});

  println("Total area: {}", total_area(shapes));

  return 0;
}
