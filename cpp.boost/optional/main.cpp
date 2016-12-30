#include <boost/optional.hpp>

#include <iostream>

using namespace std;
using namespace boost;

auto getConfigParam(string name) -> optional<int> 
{
  if (name == "MaxValue")
    return 1;
  else
    return boost::none;
}

int main(int argc, char *argv[])
{
  if (auto oi = getConfigParam("MaxValue"))
    cout << *oi;
  else 
    cout << "null";

  return 0;
}
