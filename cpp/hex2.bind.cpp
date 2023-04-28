#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <iostream>
#include <sstream>
#include <algorithm>

using namespace std;

string hex_(char v)
{
  char buf[4] = { 0 };
  sprintf(buf, "%02x ", v);

  return string(buf);
}

template <typename ForwardIterator>
string to_hex(ForwardIterator beg_, ForwardIterator end_, 
              string b="[ ", string e="]")
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
