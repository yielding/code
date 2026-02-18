#include <print>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

auto main(int argc, char *argv[]) -> int
{
  auto fruits = { "apple"s, "banana"s, "kiwi"s };

  // enumerate: associate elements with their positions
  for (auto&& [seq, fruit] : fruits | v::enumerate) 
    println("{}, {}", seq + 1, fruit);
  
  return 0;
}