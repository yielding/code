#include <vector>
#include <iostream>
#include <string>
#include <charconv>
#include <optional>
#include <ranges>

using namespace std;
using namespace literals;

template <typename T>
using vector_ex = vector<optional<T>>;

auto to_int(const string_view s) -> optional<int>
{
  if (int r; from_chars(s.data(), s.data()+s.size(), r).ec == errc{})
    return r;

  return nullopt;
}

int main(int argc, char* argv[])
{
  vector_ex<string> v = { "1234", "15 foo", "bar", "42", "5000", " 5" };

  // NOTICE
  // transform == fmap (map)
  // and_then  == bind (flatmap)
  auto filter = [](auto&& o) {     // optional<string>,
    return o.and_then(to_int)      // flatmap from str to int
            .transform([](auto n) { return n + 1; })
            .transform([](auto n) { return to_string(n); })
            .or_else  ([]         { return optional("null"s); });
  };

  for (auto&& x : v | views::transform(filter)) 
    cout << *x << '\n';

  return 0;
}