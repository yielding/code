#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <format>

using namespace std;

string hex_(char v)
{
  return format("{:02x}", static_cast<unsigned char>(v));
}

template <typename ForwardIterator>
auto to_hex(ForwardIterator beg_, ForwardIterator end_, 
            string b="[ ", string e="]") -> string
{
  using namespace boost::lambda;

  stringstream ss;
  ss << b; 
  for_each(beg_, end_, ss << bind(&hex_, _1)); 
  ss << e;

  return ss.str();
}

int main(int argc, char const *argv[])
{
  string a = "abcde";

  cout << to_hex(a.begin(), a.end())     << endl;
  cout << to_hex(a.begin(), a.begin()+5) << endl;

  return 0;
}
