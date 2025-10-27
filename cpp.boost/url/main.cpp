#include <boost/url.hpp>
#include <iostream>
#include <typeinfo>
#include <string>

using namespace std;
using namespace boost::urls;

int main() 
{
  auto u = parse_uri("ftp://1.2.3.4/path");
  if (!u) 
  { 
    cerr << "Invalid URL\n"; 
    return 1;
  }

  url_view v = *u;
  cout << "scheme: " << v.scheme() << "\n";
  cout << "host  : " << v.host() << "\n";           // "1.2.3.4"
  cout << "port  : '" << v.port() << "'\n";         // ''
  cout << "path  : " << v.encoded_path() << "\n";   // "/path"

  auto p = v.port_number();
  cout << "port_number(): [" << p << "]\n";

  cout << "has_port(): " << v.has_port() << "\n";

  auto default_p = default_port(v.scheme_id());
  cout << "default_port(): [" << default_p << "]\n";

  auto default_port_str = to_string(default_p);
  cout << "default_port() as string: [" << default_port_str << "]\n"; // "21"

  return 0;
}
