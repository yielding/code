#include <iostream>
#include <string>
#include <vector>

using namespace std;

namespace detail {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename Seperator> 
auto split_aux(string const& val, Seperator&& sep) -> vector<string>
{
  vector<string> result;
  string::size_type p = 0, q;

  while ((q = sep(val, p)) != string::npos)
  {
    result.emplace_back(val, p, q - p);
    p = q + 1;
  }

  result.emplace_back(val, p);
  return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} 

auto split(string const& val, char sep) -> vector<string>
{
  return detail::split_aux(val, 
    [=](string const& v, string::size_type p) { return v.find(sep, p); });
}

auto split(string const& val, string const& seps) -> vector<string>
{
  return detail::split_aux(val, 
    [&](string const& v, string::size_type p) {
        return v.find_first_of(seps, p);
    });
}

int main(int argc, const char *argv[])
{
  auto s0 = "/var/mobile/Applications/Kakao/Kakao.sqlite";
  auto v0 = split(s0, '/');
  cout << "size: " << v0.size() << endl;
  for (auto elem: v0) cout << elem << endl;

  auto s1 = "|var/mobile|Applications/Kakao|Kakao.sqlite";
  auto v1 = split(s1, "|/");
  cout << "size: " << v1.size() << endl;
  for (auto elem: v1) cout << elem << endl;

  return 0;
}
