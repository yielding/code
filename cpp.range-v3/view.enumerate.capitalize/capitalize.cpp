#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto capitalize(string const& s) -> string
{
  using v::enumerate, v::transform, g::to;

  auto third_upper = [](auto const& p) -> auto { 
    return p.first % 3 == 0 ? toupper(p.second) : p.second;
  };

  return s | enumerate
           | transform(third_upper)
           | to<string>;
}

auto main(int argc, char *argv[]) -> int
{
  auto r = capitalize("consequnce"s);
  assert(r == "ConSeqUncE");
  
  return 0;
}