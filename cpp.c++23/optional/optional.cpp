#include <vector>
#include <iostream>
#include <string>
#include <charconv>
#include <optional>
#include <ranges>

using namespace std;
using namespace literals;

auto to_int(string_view s) -> optional<int> 
{
  if (int r; from_chars(s.data(), s.data()+s.size(), r).ec == errc{})
    return r;

  return nullopt;
}

int main()
{
  vector<optional<string>> v = {
    "1234", "15 foo", "bar", "42", "5000000000", " 5" 
  };

  auto res = v | views::transform([](auto&& o) { 
    return o.and_then(to_int) // flatmap from str to int
            .transform([](int n) { return n + 1; })
            .transform([](int n) { return to_string(n); })
            .or_else([] { return optional{"null"s}; }); });

  for (auto&& x : res) 
    cout << *x << '\n';

  return 0;
}