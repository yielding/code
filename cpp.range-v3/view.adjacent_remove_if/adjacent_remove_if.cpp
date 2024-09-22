#include <iostream>

#include <range/v3/all.hpp>

namespace v = ranges::view;
using namespace std;

int main(int argc, char* argv[])
{
  auto v = vector{1, 2, 3, 3, 4, 4};
  auto r = v | v::adjacent_remove_if(equal_to{});

  cout << v::all(r) << endl; // [1, 2, 3, 4]

  return 0;
}