#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char *argv[])
{
  auto fruits = { "apple"s, "banana"s, "kiwi"s };

  for (auto&& [seq, fruit] : fruits | v::enumerate) 
    cout << seq + 1 << ", " << fruit << endl;
  
  return 0;
}