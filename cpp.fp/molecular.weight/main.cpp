#include <iostream>
#include <string>
#include <sstream>
#include <optional>
#include <map>
#include <regex>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;
using g::to;

auto diff(string const& s1, string const& s2)
{
  auto s = v::set_difference(s1, s2) | to<string>;

  int i; istringstream iss(s); iss >> i;

  return !iss.fail() 
    ? make_optional(i)
    : nullopt;
}

template <typename Map, typename Key>
auto lookup(Map const& m, Key const& k)
{
  auto it = m.find(k);

  return it != m.end()
    ? make_optional(it->second)
    : nullopt;
}

int main(int agc, char* agv[])
{
  auto const s = string{"H2O"};
  auto const m = map<string, double> {
    {"O", 15.999}, 
    {"H",  1.008}, 
    {"C", 12.011},
  };

  auto seq = s | v::tokenize(regex{"[A-Z][a-z]?[0-9]*"});
  auto elm = s | v::tokenize(regex{"[A-Z][a-z]?"});

  auto masses = elm 
    | v::for_each([m](auto const& s) {
        auto w = lookup(m, s);
        return g::yield_if(w.has_value(), 
                        w.value_or(0.0));
        }); 

  auto coeff = v::zip_with([](auto const& s1, auto const& s2) {
        return diff(s1, s2).value_or(1); 
      }, seq, elm);

  auto valid = (g::distance(masses) == g::distance(coeff));

  auto val = valid
    ? to_string(g::inner_product(masses, coeff, 0.0))
    : "Invalid input"s;

  //cout << *diff("H21", "H");
  cout << v::all(seq) << endl;
  cout << v::all(elm) << endl;
  cout << v::all(masses) << endl;
  cout << v::all(coeff) << endl;
  cout << valid << endl;
  cout << val << endl;

  return 0;
}