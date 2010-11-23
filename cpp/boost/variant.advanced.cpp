#include <cstddef>
#include <iostream>
#include <boost/variant.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/contains.hpp>
#include <boost/utility/enable_if.hpp>

// Generic visitor that does magical dispatching of
// types and delegates passes down to your visitor only
// those types specified in a type list.

using namespace boost;
using namespace boost::mpl;

template <typename Visitor, typename TypeList>
struct picky_visitor: static_visitor<void>, Visitor
{
  template <typename T>
  void operator() (T v, 
      typename enable_if<typename contains<TypeList, T>::type>::type* dummy=NULL) const
  {
    Visitor::operator () (v);
  }

  template <typename T>
  void operator() (T v, 
      typename disable_if<typename contains<TypeList, T>::type>::type* dummy=NULL) const
  {}
};

// Usage example:

struct Usage 
{
  int x;
};

struct  nil {};
typedef variant<nil, char, int, double, Usage> sql_field;

struct example_visitor
{
  typedef picky_visitor<example_visitor, mpl::vector<char, int, double, Usage>> value_type;

  void operator() (char v) const
  {
    std::cout << "character detected" << std::endl;
  }

  void operator() (int v) const
  {
    std::cout << "integer detected" << std::endl;
  }

  void operator() (double v) const
  {
    std::cout << "double detected" << std::endl;
  }

  void operator() (Usage v) const
  {
    std::cout << "Usage detected" << std::endl;
  }
};

int main(int argc, char* argv[])
{
  example_visitor::value_type visitor;

  sql_field nilField;
  sql_field charField ('X');
  sql_field intField (1986);
  sql_field doubleField (19.86);

  Usage u;
  sql_field usage = u;

  boost::apply_visitor (visitor, nilField);
  boost::apply_visitor (visitor, charField);
  boost::apply_visitor (visitor, intField);
  boost::apply_visitor (visitor, doubleField);
  boost::apply_visitor (visitor, usage);

  return 0;
}
