#include <iostream>
#include <sstream>
#include <cassert>

#include <array>
#include <string>
#include <vector>
#include <tuple>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>

#include <boost/format.hpp>
#include <msgpack.hpp>

using namespace std;
using namespace boost;
using namespace msgpack;

template <typename T>
void serialize_test(string const& str, T& t, size_t& offset)
{
  auto ohd = unpack(str.data(), str.size(), offset);
  auto obj = ohd.get();
  cout << obj << endl;
  assert(obj.as<T>() == t);

  cout << format("offset: %s\n") % offset;
}

int main(int argc, char* argv[])
{
  std::array<int, 5>         a { { 1, 2, 3, 4, 5 } };
  tuple<bool, string, int>   t { true, "ABC", 42 };
  unordered_map<string, int> m { {"ABC", 1}, {"DEF", 3} };
  unordered_set<string>      s { "ABC", "DEF" };
  forward_list<string>       f { "ABC", "DEF" };

  stringstream ss;
  pack(ss, a); pack(ss, t); pack(ss, m);
  pack(ss, s); pack(ss, f);

  size_t offset = 0;
  auto s_ = ss.str();
  serialize_test(s_, a, offset);
  serialize_test(s_, t, offset);
  serialize_test(s_, m, offset);
  serialize_test(s_, s, offset);
  serialize_test(s_, f, offset);

  return 0;
}