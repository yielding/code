#include <iostream>
#include <string>
#include <cctype>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int agc, char* agv[])
{
  auto const& s = "A Man, A Plan, A Canal - Panama!"s;

  auto lowered = s 
    | v::transform([](auto ch) { return tolower(ch); })
    | v::filter   ([](auto ch) { return isalpha(ch); })
    ;
  
  auto reversed = lowered | v::reverse; 
  cout << reversed << endl;

  const int n = g::distance(reversed) / 2;
  cout << g::equal(lowered | v::take(n), reversed | v::take(n));

  return 0;
}