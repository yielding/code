#include <iostream>
#include <map>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto const m = map<string, string> {{"red","#ff0000"},{"green","#00ff00"}};

  auto rng = m | v::addressof;
  cout << v::all(rng);

  return 0;
}
