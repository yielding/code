#include <iostream>
#include <sstream>
#include <range/v3/all.hpp>

//
// Prob) Planetary masses
// Determine the ratio between the mass of the heaviest planet 
// and mass of all remaining planets combined
//
namespace v = ranges::views;
namespace g = ranges;

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
  double d; istringstream iss(s);
  iss >> d;
  
  return !iss.fail() 
    ? make_optional(d) 
    : nullopt;
}

auto split_line(string const& s) -> vector<string>
{
  using v::split, v::remove_if, g::to;

  return s 
    | remove_if([](auto c) { return isspace(c); })
    | split(',')
    | to<vector<string>>();
}

auto make_planet(string const& s) -> optional<planet>
{
  auto v = split_line(s);
  if (g::distance(v) >= 2)
  {
    auto op = to_double(v[1]);
    if (op.has_value())
      return make_optional(planet{v[0], *op});
  }

  return nullopt;
}

int main(int agc, char* agv[])
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

  auto rng = g::getlines(iss);

  auto planets = rng 
    | v::for_each([](auto const& s) {
        auto op = make_planet(s);
        return g::yield_if(op.has_value(), op.value_or(planet{})); })
    | g::to<vector>;

  g::sort(planets, greater{}, &planet::mass);

  auto tail = g::accumulate(planets | v::tail, 0.0, {}, &planet::mass);
  auto eps = numeric_limits<double>::epsilon();
  auto rat = (!empty(planets) && tail > eps)
    ? to_string(g::front(planets).mass / tail)
    : "Invalid Input"s;

  cout << rat << endl;

  return 0;
}