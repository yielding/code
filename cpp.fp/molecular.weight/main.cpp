#include <iostream>
#include <string>
#include <sstream>
#include <optional>
#include <map>
#include <regex>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

using namespace std;
using rg::to;


auto diff(string const& s1, string const& s2)
{
  auto s = rv::set_difference(s1, s2)
    | to<string>;

  int i;
  istringstream iss(s);
  iss >> i;

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

int main(int argc, char* argv[])
{
  auto const s = string{"H2O"};
  auto const m = map<string, double> {
    {"O", 15.999}, 
    {"H",  1.008}, 
    {"C", 12.011},
  };

  auto seq = s 
    | rv::tokenize(regex{"[A-Z][a-z]?[0-9]*"});

  auto elm = s 
    | rv::tokenize(regex{"[A-Z][a-z]?"});

  auto masses = elm 
    | rv::for_each([m](auto const& s) {
        auto w = lookup(m, s);
        return rg::yield_if(w.has_value(), 
                        w.value_or(0.0));
        }); 

  auto coeff = rv::zip_with([](auto const& s1, auto const& s2) {
        return diff(s1, s2).value_or(1); 
      }, seq, elm);

  auto valid = (rg::distance(masses) == rg::distance(coeff));

  auto val = valid
    ? to_string(rg::inner_product(masses, coeff, 0.0))
    : "Invalid input";

  //cout << *diff("H21", "H");
  cout << rv::all(seq) << endl;
  cout << rv::all(elm) << endl;
  cout << rv::all(masses) << endl;
  cout << rv::all(coeff) << endl;
  cout << valid << endl;
  cout << val << endl;

  return 0;
}