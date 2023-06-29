#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <cassert>

#include <msgpack.hpp>  

using namespace msgpack;
using namespace std;

int main(int argc, char* argv[])
{
  map<string, vector<int>> v1 {
    { "abc", {1, 2, 3} }, 
    { "가나다", {10, 9, 8} } 
  };

  vector<string> v2 = { 
    "HIJ", "KLM", "NOP"
  };

  // two data structures are serialized
  stringstream ss; 
  pack(ss, v1); 
  pack(ss, v2);

  string res(ss.str()); 
  cout << format("size: {}\n", res.size());

  size_t off = 0;
  auto o1 = unpack(res.data(), res.size(), off).get();
  cout << "content: " << o1 << endl
       << "offset: " << off << endl;

  auto const& r1 = o1.as<decltype(v1)>();
  for (auto& e: r1) cout << e.first << " ";
  cout << endl;

  auto o2 = unpack(res.data()+off, res.size()).get();
  cout << o2 << endl;
  auto const& r2 = o2.as<decltype(v2)>();
  assert(r2[1] == "KLM");

  return 0;
}