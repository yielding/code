#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace boost::archive::iterators;

string decode64(const string &val) 
{
  using It = transform_width<binary_from_base64<string::const_iterator>, 8, 6>;
  return boost::algorithm::trim_right_copy_if(string(It(begin(val)), It(end(val))), 
      [](char c) { return c == '\0'; });
}

string encode64(const string &val) 
{
  using It = base64_from_binary<transform_width<string::const_iterator, 6, 8>>;
  auto tmp = string(It(begin(val)), It(end(val)));
  return tmp.append((3 - val.size() % 3) % 3, '=');
}

int main(int argc, char *argv[])
{
  return 0;
}
