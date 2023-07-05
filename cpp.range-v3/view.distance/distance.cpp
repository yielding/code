#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

void accumulate_practice()
{
  string const s = "we have been better days";
  auto rng = s
    | v::split(' ')
    | v::transform([](auto && s) { return g::distance(s); })
    ;

  auto avg = g::accumulate(rng, 0) / g::distance(rng);
  
  cout << v::all(rng) << endl;
  cout << avg << endl;
}

int main(int argc, char* argv[])
{
  accumulate_practice();

  return 0;
}