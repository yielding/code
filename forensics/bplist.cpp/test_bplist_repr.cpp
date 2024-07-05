#include "catch2/catch.hpp"

#include "bplist_repr.hpp"

//#include <format>
#include <iostream>
#include <sstream>

using namespace std;
using namespace boost;

using namespace util::parser;

struct value_adder
{
  typedef boost::mpl::vector<CFInteger, CFString, CFArray> filter;
  typedef generic_visitor<value_adder, filter> visitor_type;

  value_adder() : m_total(0)
  {}

  void operator() (CFString& s) const
  {
    //m_repr += format("{}", s.value());
    m_repr += s.value();
  }

  void operator() (CFInteger& i) const
  {
    //m_repr  += format("{}", i.value());
    m_repr  += to_string(i.value());
    m_total += i.value();
  }

  void operator() (CFArray& v) const
  {
    m_repr += "(";
    visitor_type visitor;

    for (size_t i=0; i<v.size(); i++)
      apply_visitor(visitor, v[i]);

    m_total += visitor.result();
    m_repr  += visitor.repr();

    m_repr += ")";
  }

  int result() { return m_total; }

  string repr() { return m_repr; }

private:
  mutable int m_total;
  mutable string m_repr;
};

TEST_CASE("bplist_repr") {
  SECTION("recursive wrapper") {
    CFArray c;
    c.add(CFInteger(100));
    c.add(CFInteger(200));
    c.add(CFString("leech"));
    c.add(c);
    c.add(CFInteger(50));
    c.add(c);
    CFType root = c;

    value_adder::visitor_type v;
    boost::apply_visitor(v, root);

    REQUIRE(v.result() == 1300);
    REQUIRE(v.repr() == "(100200leech(100200leech)50(100200leech(100200leech)50))");
  }
}
