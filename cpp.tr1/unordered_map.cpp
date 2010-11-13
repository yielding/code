#include <boost/unordered_map.hpp>
#include <cassert>

int main(int argc, char const* argv[])
{
  typedef boost::unordered_map<std::string, int> map;

  map x;

  x["one"] = 1;
  x["two"] = 2;
  x["three"] = 3;

  assert(x.at("one") == 1);
  assert(x.at("two") == 2);
  assert(x.find("missing") == x.end());

  return 0;
}
