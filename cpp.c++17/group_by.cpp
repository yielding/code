#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include <map>
#include <type_traits>

using namespace std;

namespace fun {
  template <typename C, typename F>
    auto group_by(C&& c, F f) 
    {
      using element = decay_t<decltype(*data(c))>;
      using desc    = decltype(f(*data(c)));
      map<desc, vector<element>> r;

      for (auto&& e : c) 
        r[f(e)].emplace_back(e);

      return r;
    }

  template <typename C, typename F>
    auto map(C&& c, F f)
    {
      using key_t = decltype(f(*begin(c)).first);
      using val_t = decltype(f(*begin(c)).second);

      map<key_t, val_t> r;
      for (auto&& e : c)
      {
        auto&& mapped = f(e);
        r[mapped.first] = mapped.second;
      }

      return r;
    }
}

template<class Ch, class Tr, class T>
auto operator<<(basic_ostream<Ch, Tr>& os, vector<T> const& v)
  -> basic_ostream<Ch, Tr>& 
{
  os << "{";
  for (size_t i = 0; i < v.size(); ++i) 
  {
    os << v[i];
    if (i != v.size() - 1)
      os << ", ";
  }

  os << "}";
  return os;
}

// pair 타입 출력
template<class Ch, class Tr, class T1, class T2>
auto operator<<(basic_ostream<Ch, Tr>& os, pair<T1, T2> const& p)
  -> basic_ostream<Ch, Tr>& 
{
  return os << "<" << p.first << ", " << p.second << ">";
}

int main(int argc, char *argv[])
{
  auto f = fun::map(
      fun::group_by(string("Hello"),[](auto e) { return e; }),
      [](auto p) { return make_pair(p.first, p.second.size()); });

  for (auto e : f)
    cout << e << endl;

  return 0;
}
