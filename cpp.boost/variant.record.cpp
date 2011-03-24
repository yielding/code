#include "boost/variant.hpp"
#include <iostream>

using namespace std;

struct Result
{
  Result(int v): value(v) {}
  int value;
};

struct Source1
{
  int    offset() { return 1; }
  Result result() { Result r(11); return r; }
};

struct Source2
{
  int    offset() { return 2; }
  Result result() { Result r(22); return r; }
};

struct Source3
{
  int    offset() { return 3; }
  Result result() { Result r(33); return r; }
};

typedef boost::variant<Source1, Source2, Source3> DataSource;

struct offset_visitor: public boost::static_visitor<int> {
  template <typename T> 
  int operator()(T& t) const { return t.offset(); }
};

struct result_visitor: public boost::static_visitor<Result> {
  template <typename T> 
  Result operator()(T& t) const { return t.result(); }
};

class Record
{
public:
  template <typename T> 
  Record(T const& t) : m_src(t)
  {}

  int offset()
  {
    return boost::apply_visitor(offset_visitor(), m_src);
  }

  int result()
  {
    return boost::apply_visitor(result_visitor(), m_src).value;
  }

private:
  DataSource m_src;
};

int main(int argc, char const* argv[])
{
  Source1 s1; Source2 s2; Source3 s3;

  Record r1(s1); cout << r1.offset() << " " << r1.result() << endl;
  Record r2(s2); cout << r2.offset() << " " << r2.result() << endl;
  Record r3(s3); cout << r3.offset() << " " << r3.result() << endl;

  return 0;
}
