#include <cmath>    // for isinf
#include <iomanip>  // for quoted
#include <expected>
#include <iostream>

using namespace std;

enum class parse_error 
{
  invalid_input,
  overflow
};

auto parse_number(string_view str) -> expected<double, parse_error>
{
  auto beg = str.data();
  char* end;
  auto retval = strtod(beg, &end); // string to double

  if (beg == end)
    return unexpected(parse_error::invalid_input);

  if (isinf(retval))
    return unexpected(parse_error::overflow);
  
  str.remove_prefix(end - beg);

  return retval;
}

auto times_10(double d) -> expected<double, parse_error>
{
  return d * 10;
}

auto print_error(parse_error e) -> expected<double, string>
{
  switch (e)
  {
    case parse_error::invalid_input:
      return unexpected("error : invalid input");

    case parse_error::overflow:
      return unexpected("error : overflow");

    default:
      return unexpected("error");
  }
}

// NOTE
// 1) transform : fmap 
//    E double -> (double -> double) -> E double      (a -> b) -> f a -> f b
//
// 2) and_then : bind
//    E double -> (double -> E double) -> E double    m a -> (a -> m b) -> m b
int main(int argc, char* argv[])
{
  for (auto const& src: { "42", "43 abc ", "meow", "inf" })
  {
    auto res = parse_number(src)
                .transform([](double d) { return d + 10; }) // transform: fmap(functor)double -> double
                .and_then(times_10)                         // and_then
                .or_else(print_error);

    if (res.has_value())
      cout << *res << endl;
    else 
      cout << res.error() << endl;
  }

  return 0;
}