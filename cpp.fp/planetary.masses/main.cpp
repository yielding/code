#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <optional>
#include <range/v3/all.hpp>

//
// Prob) Planetary masses
// Determine the ratio between the mass of the heaviest planet 
// and mass of all remaining planets combined
//
namespace rv = ranges::views;
namespace rg = ranges;
using namespace std;

struct planet 
{
  string name;
  double mass;

  friend ostream& operator << (ostream& os, planet& p)
  {
    os << p.name << " " << p.mass;
    return os;
  }
};

// NOTICE
// spirit parser is algo very good. but for me, easier is better
auto to_double(string const& s)
{
  double d;
  istringstream iss(s);
  iss >> d;
  
  return !iss.fail() 
    ? make_optional(d) 
    : nullopt;
}

auto split_line(string const& s) -> vector<string>
{
  return s 
    | rv::remove_if([](auto c) { return isspace(c); })
    | rv::split(',')
    | rg::to<vector<string>>();
}

auto make_planet(string const& s) -> optional<planet>
{
  auto v = split_line(s);
  if (rg::distance(v) >= 2)
  {
    auto op = to_double(v[1]);
    if (op.has_value())
      return make_optional(planet{v[0], *op});
  }

  return nullopt;
}

int main(int argc, char* argv[])
{
  auto txt = R"(Earth,1.0
                Mercury,0.0553 
                Venus,0.815 
                Mars,0.107 
                Jupiter,317.8 
                Saturn,95.2 
                InvalidPlanet1, 
                InvalidPlanet2,abc 
                Uranus,14.5 
                Neptun, 17.2)";

  istringstream iss{txt};

  auto rng = rg::getlines(iss);

  auto planets = rng 
    | rv::for_each([](auto const& s) {
        auto op = make_planet(s);
        return rg::yield_if(op.has_value(), op.value_or(planet{})); })
    | rg::to<vector>;

  rg::sort(planets, greater{}, &planet::mass);

  auto tail = rg::accumulate(planets | rv::tail, 0.0, {}, &planet::mass);
  auto  eps = numeric_limits<double>::epsilon();
  auto  rat = (!empty(planets) && tail > eps)
    ? to_string(rg::front(planets).mass / tail)
    : "Invalid Input"s;

  cout << rat << endl;

  return 0;
}