#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto const v = vector{ make_shared<int>(1), make_shared<int>(2) };

  auto rng = v | v::indirect; // [1, 2]
  cout << v::all(rng);

  return 0;
}
