#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/lambda/lambda.hpp>

#include <iostream>
#include <string>
#include <vector>
#include <complex>

using namespace std;
using namespace boost::phoenix;
using namespace boost::spirit;
using namespace boost::spirit::qi;
using namespace boost::spirit::ascii;
using namespace boost::spirit::arg_names;

struct writer
{
  void operator()(int const& i, unused_type, unused_type) const
  {
    cout << i << endl;
  }
};

template <typename Iterator>
bool parse_complex2(Iterator first, Iterator last, std::complex<double>& c)
{
  double r = 0.0;
  double i = 0.0;
  double res = phrase_parse(first, last,
    (
      '(' >> double_[ref(r) = _1]
          >> -(',' >> double_[ref(i) = _1]) >> ')'
      | double_[ref(r) = _1]
    ),
    space
  );
  
  if (!res || first != last)
    return false;
  
  c = std::complex<double>(r, i);
  return res;
}

template <typename Iterator>
bool parse_numbers2(Iterator first, Iterator last, vector<double>& v)
{
  bool r = phrase_parse(first, last,
    double_[push_back(ref(v), _1)] 
      >> *(',' >> double_[push_back(ref(v), _1)]),
    space);
  
  if (first != last)
    return false;
    
  return r;
}

template <typename Iterator>
bool parse_numbers3(Iterator first, Iterator last, vector<double>& v)
{
  bool r = phrase_parse(first, last, 
    double_[push_back(ref(v), _1)] % ',', space);
  
  if (first != last)
    return false;
    
  return r;
}

template <typename Iterator>
bool parse_numbers4(Iterator first, Iterator last, vector<double>& v)
{
  bool r = phrase_parse(first, last, double_ % ',', v, space);
  
  if (first != last)
    return false;
    
  return r;
}

int main(int argc, char const *argv[])
{
  {
  // char const* s0 = "{5}";
  // qi::parse(s0, s0+4,'{' >> int_[writer()] >> '}' );
  }
  
  {
  // string s1 = "( 1.0, 2.0  )";
  // std::complex<double> c;
  // parse_complex2(s1.begin(), s1.end(), c);
  // cout << c;
  }
  
  {
  string ints="1,2,3              ,4,5.2";
  vector<double> v;
  if (!parse_numbers4(ints.begin(), ints.end(), v))
    cout << "parsing ints error\n";
  
  cout << "sum of v: " 
       << accumulate(v.begin(), v.end(), 0.0) << endl;
  copy(v.begin(), v.end(), ostream_iterator<double>(cout, " "));
  }
  
  return 0;
}
