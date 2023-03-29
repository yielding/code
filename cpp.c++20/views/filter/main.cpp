#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/filter.hpp>

namespace view = ranges::views;

int main(int argc, char *argv[])
{
  using namespace std;

  auto v = { 7, 4, 2, 6, 9 };
  auto rng = v | view::filter([](int x) { return x > 6; }) ;

  cout << view::all(rng);
  
  return 0;
}