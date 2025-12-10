#include <iostream>
#include <string>
#include <functional>

#include <boost/asio.hpp>

namespace asio = boost::asio;
namespace ip   = boost::asio::ip;

using namespace std;

const short multicast_port = 30001;

int remainder__ = 0;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class receiver
{
public:
  receiver(asio::io_context& io_context, ip::address const& addr)
    : _socket(io_context)
  {
    ip::udp::endpoint listen_endpoint(ip::make_address("0.0.0.0"), multicast_port);
    _socket.open(listen_endpoint.protocol());
    _socket.set_option(ip::udp::socket::reuse_address(true));
    _socket.bind(listen_endpoint);

    _socket.set_option(ip::multicast::join_group(addr));
    _socket.async_receive_from(asio::buffer(_data, max_length), _sender_endpoint,
      bind_front(&receiver::handle_receive_from, this));
  }

  void handle_receive_from(const boost::system::error_code& error, size_t)
  {
    if (error)
    {
      cout << "connection closed\n";
      return;
    }

    auto count = atoi(_data);
    if (count % 10 == remainder__)
      cout << "my seq: " << count << endl;

    _socket.async_receive_from(asio::buffer(_data, max_length), _sender_endpoint,
      bind_front(&receiver::handle_receive_from, this));
  }

private:
  ip::udp::socket _socket;
  ip::udp::endpoint _sender_endpoint;
  enum { max_length = 1024 };
  char _data[max_length];
};

int main(int argc, char* argv[])
{
  try
  {
    auto multicast_address = "239.255.0.1"s;
    if (argc != 2)
      return EXIT_FAILURE;

    remainder__ = atoi(argv[1]);
    cout << "remainder : " << remainder__ << endl;

    asio::io_context io_context;
    receiver r(io_context, ip::make_address(multicast_address));
    io_context.run();
  }
  catch (exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
