#include <iostream>
#include <vector>
#include <format>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto v = vector{7, 0, 2, 3, 1, 4, 6}; 

  auto rng = v 
    | v::enumerate 
    | v::for_each([](auto const& p) {
        return g::yield_if(p.first == p.second, 
                           p.second);
      });

  cout << v::all(rng) << endl;

  return 0;
}