#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/zlib.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;
      namespace io = boost::iostreams;

template <class cha_type, class iterator_type>
struct my_source 
{
  typedef cha_type char_type;
  typedef io::source_tag category;

  iterator_type& it;
  iterator_type end;

  my_source(iterator_type& it, iterator_type end = {}) : it(it), end(end) 
  {}

  auto read(char* s, streamsize n) -> streamsize
  {
    streamsize result = 0;
    while (it != end && n--) 
    {
      ++result;
      *s++ = *it++;
    }

    return result;
  }
};

int main() 
{
  string const rawdata {
    'x', '\234', '\313', 'H', '\315', '\311', 
    '\311', 'W', '(', '\317', '/', '\312', 'I', 
    '\341', '\002', '\0', '\036', 'r', '\004', 'g' };

  istringstream iss(rawdata, ios_base::binary);

  /*
  // my_source는 사용자에게 더 많은 제어권을 준다
  //
  auto start = istreambuf_iterator<char>(iss);
  my_source<char, decltype(start)> data(start);

  io::filtering_istreambuf def;
  def.push(io::zlib_decompressor());
  def.push(data);
  */

  io::filtering_streambuf<io::input> in;
  in.push(io::zlib_decompressor());
  in.push(iss);

  // io::copy(in, cout);
  
  // vector<char>에는 char_type 내부적으로 없기 때문에 사용할 수 없다.
  stringstream oss;
  io::copy(in, oss);
  cout << oss.str();

  return 0;
}
