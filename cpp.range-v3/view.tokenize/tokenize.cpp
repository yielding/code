#include <iostream>
#include <cassert>

#include <range/v3/all.hpp>

namespace v = ranges::view;

using namespace std;

int main(int argc, char* argv[])
{
  auto s0 = string{"A nice     day!"};
  auto rx = regex{"[\\w]+"};
  auto rg = s0 | v::tokenize(rx);

  cout << v::all(rg) << endl;
}