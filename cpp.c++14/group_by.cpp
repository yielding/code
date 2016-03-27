#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;

namespace fun {
  template <typename C, typename F>
  auto group_by(C&& c, F f) {
    using element = decay_t<decltype(*data(c))>;
    using desc    = decltype(f(data(c)));
    map<desc, vector<element>> r;

    for (auto&& e : c) r[f(e)].emplace_back(e);

    return r;
  }
}

int main(int argc, char *argv[])
{
  
  return 0;
}
