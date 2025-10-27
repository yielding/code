#include <boost/url.hpp>
#include <iostream>
#include <typeinfo>

int main()
{
  auto u = boost::urls::parse_uri("http://1.2.3.4/path");
  boost::urls::url_view v = *u;

  auto port_num = v.port_number();
  std::cout << "Type: " << typeid(port_num).name() << "\n";
  std::cout << "Port value: " << port_num << "\n";

  return 0;
}
