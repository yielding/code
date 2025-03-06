#include <iostream>
#include <boost/asio.hpp>

using boost::asio::ip::tcp;
using namespace std;
using namespace boost;

int main(int argc, const char *argv[])
{
  asio::io_context ios;

  tcp::resolver resolver(ios);

  // query는 ip, server name, port, service name 모두 다 동작하는 군.
  auto endpoints = resolver.resolve(tcp::v4(), "218.145.28.200", "80");

  tcp::socket socket(ios);
  asio::connect(socket, endpoints);
  cout << "Connected\n";

  auto req = "GET\n\r";
  asio::write(socket, asio::buffer(req, 5));
  asio::streambuf response;
  asio::read_until(socket, response, "\r\n\r\n");
  if (response.size() > 0)
    cout << &response;

  return 0;
}
