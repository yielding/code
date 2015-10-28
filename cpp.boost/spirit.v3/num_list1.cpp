#include <boost/spirit/home/x3.hpp>
#include <iostream>
#include <string>
#include <vector>

namespace x3    = boost::spirit::x3;
namespace ascii = boost::spirit::x3::ascii;

template <typename Iterator>
bool parse_numbers(Iterator first, Iterator last, std::vector<double>& v)
{
  auto doubles = x3::rule<class doubles, std::vector<double>>()
               = x3::double_ % ',';

  auto result = x3::phrase_parse(
      first, last,
      doubles,
      ascii::space,
      v
  );

  return first == last 
    ? result 
    : false;
}

using namespace std;

int main(int argc, char *argv[])
{
  vector<double> result;

  auto line = "1.1, 2.2 ,  3.3"s;

  if (parse_numbers(line.begin(), line.end(), result))
  {
    cout << "ok" <<endl;
    for (auto d : result) cout << d << endl;
  }
  else
  {
    cout << "fail" <<endl;
  }

  return 0;
}
