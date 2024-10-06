#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int hamming_distance(auto& a1, auto& a2)
{
  auto diff = v::zip_with(bit_xor{}, a1, a2); // [1, 1, 1, 0]

  return g::accumulate(diff, 0);
}

int main(int argc, char* argv[])
{
  auto const ar1 = array<uint8_t, 4> {1, 0, 1, 0};
  auto const ar2 = array<uint8_t, 4> {0, 1, 0, 0};

  cout << hamming_distance(ar1, ar2);
  
  return 0;
}
