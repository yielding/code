#include <iostream>
#include <string>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
using namespace std;

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
  auto const& mountains = vector<mountain> {
    { "aconcagua",  6961. }, 
    { "parinacota", 6342. },
    { "licancabur", 5920. }, 
    { "guallatiri", 6071. }
  };

  auto rng = mountains 
           | v::filter([](double h) { return h > 6200.0; }, &mountain::height);

  cout << v::all(rng);

  return 0;
}