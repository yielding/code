#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  using v::tokenize, v::all;

  auto s0 = "A nice     day!"s;
  auto rx = regex{"[\\w]+"};
  auto rg = s0 | tokenize(rx);

  cout << all(rg) << endl;
}