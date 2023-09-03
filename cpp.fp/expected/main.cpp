#include <cmath>    // for isinf
#include <iomanip>  // for quoted
#include <expected>
#include <iostream>
#include <vector>

using namespace std;

/*! \enum class rparse_error
 *
 *  string parse error
 */
enum class parse_error {
  invalid_input,
  overflow
};

auto parse_number(string_view str) -> expected<double, parse_error>
{
  const char* beg = str.data();
  char* end;
  auto retval = strtod(beg, &end);

  if (beg == end)
    return unexpected(parse_error::invalid_input);

  if (isinf(retval))
    return unexpected(parse_error::overflow);
  
  str.remove_prefix(end - beg);

  return retval;
}

auto print_error(parse_error e) -> expected<double, string>
{
  if (e == parse_error::invalid_input)
    return unexpected("error : invalid input");

  if (e == parse_error::overflow)
    return unexpected("error : overflow");

  return unexpected("error");
}

int main(int argc, char* argv[])
{
  for (auto const& src: { "42", "43 abc ", "meow", "inf" })
  {
    auto res = parse_number(src)
                .transform([](double d) { return d + 10; })
                .or_else(print_error);

    if (res.has_value())
      cout << *res << endl;
    else 
      cout << res.error() << endl;
  }

  /*
  auto process = [](string_view str)
  {
    cout << "str: " << quoted(str) << ", ";
    if (const auto num = parse_number(str); num.has_value())
    {
      cout << "value: " << *num << '\n';
      // If num did not have a value, dereferencing num
      // would cause an undefined behavior, and
      // num.value() would throw bad_expected_access.
      // num.value_or(123) uses specified default value 123.
    }
    else if (num.error() == parse_error::invalid_input)
    {
      cout << "error: invalid input\n";
    }
    else if (num.error() == parse_error::overflow)
    {
      cout << "error: overflow\n";
    }
    else
    {
      cout << "unexpected!\n"; // or invoke unreachable();
    }
  };

  for (auto const& src: { "42", "43 abc ", "meow", "inf" })
    process(src);
  */

  return 0;
}