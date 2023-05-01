#include <iostream>
#include <string>
#include <vector>
#include <range/v3/all.hpp>

namespace rv = ranges::views;

using std::string, std::vector, std::ostream,
      std::cout, std::endl;

struct mountain
{
  string name;
  double height;

  friend ostream& operator <<(ostream& os, const mountain& m)
  {
    os << "(" << m.name << ", " << m.height << ")";
    return os;
  }
};

int main(int argc, char* argv[])
{
  auto const& v = vector<mountain> {
    {"aconcagua",  6961.}, 
    {"parinacota", 6342.},
    {"licancabur", 5920.}, 
    {"guallatiri", 6071.}
  };

  auto rng = v | rv::filter([](double h) { return h > 6200.0; }, 
                            &mountain::height);

  cout << rv::all(rng);

  return 0;
}