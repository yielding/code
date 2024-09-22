#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto capitalize(string const& s) -> string
{
  using v::enumerate, v::transform, g::to;

  auto third_upper = [](auto const& p) { 
    return p.first % 3 == 0 ? toupper(p.second) : p.second;
  };

  return s | enumerate
           | transform(third_upper)
           | to<string>;
}

int main(int argc, char *argv[])
{
  auto r = capitalize("consequnce"s);
  assert(r == "ConSeqUncE");
  
  return 0;
}