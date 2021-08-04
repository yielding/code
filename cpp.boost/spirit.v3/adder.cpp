#include <iostream>
#include <string>
#include <boost/spirit/home/x3.hpp>

using namespace std;

namespace client
{
  namespace x3 = boost::spirit::x3;
  namespace ascii = boost::spirit::x3::ascii;

  using x3::double_;
  using ascii::space;
  using x3::_attr;

  template <typename Iterator>
  auto adder(Iterator f, Iterator l, double& n) -> double
  {
    auto assign = [&] (auto& ctx) { n  = _attr(ctx); };
    auto add    = [&] (auto& ctx) { n += _attr(ctx); };

    bool r = x3::phrase_parse(f, l,
        (
        double_[assign] >> *(',' >> double_[add])
        ),
        space);

    return f == l ? r : false;
  }
}

int main()
{
  string in = "1  , 2, 3, 4, 5, 6, 7, 8,  9,    10";
  if (double n; client::adder(in.begin(), in.end(), n))
    cout << "sum: " << n << endl;
  else 
    cout << "sth wrong happened";

  return 0;
};
