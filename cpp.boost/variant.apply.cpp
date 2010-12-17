#include <boost/variant.hpp>
#include <iostream>
#include <string>

class my_visitor: public boost::static_visitor<int>
{
public:
  int operator()(int i) const
  {
    return i;
  }

  int operator()(std::string const& str) const
  {
    return str.length();
  }
};

class times_two_visitor: public boost::static_visitor<>
{
public:
  template <typename T>
  void operator()(T& operand) const
  {
    operand += operand;
  }
};

int main(int argc, char const* argv[])
{
  boost::variant<int, std::string> u;
  times_two_visitor visitor;
  
  u = 9;
  boost::apply_visitor(visitor, u);
  std::cout << u << std::endl;

  u = "Hello world";
  boost::apply_visitor(visitor, u);
  std::cout << u << std::endl;

  return 0;
}
