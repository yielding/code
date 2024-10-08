#include <iostream>

#include <range/v3/all.hpp>

namespace v = ranges::view;

using namespace std;

int main(int argc, char* argv[])
{
  using v::adjacent_remove_if, v::all;

  auto v = {1, 2, 3, 3, 4, 4};
  auto r = v | adjacent_remove_if(equal_to{});

  cout << v::all(r) << endl; // [1, 2, 3, 4]

  return 0;
}