#include <boost/asio.hpp>
#include <boost/bind.hpp> 
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>
#include <vector>

namespace asio   = boost::asio;
namespace ip     = boost::asio::ip;
namespace sys    = boost::system;
namespace holder = boost::asio::placeholders;

using namespace std;

const short multicast_port = 30001;

int remainder__ = 0;

class receiver
{
public:
  receiver(asio::io_service& io_service, ip::address const& addr)
    : _socket(io_service)
  {
    ip::udp::endpoint listen_endpoint(ip::address::from_string("0.0.0.0"), multicast_port);
    _socket.open(listen_endpoint.protocol());
    _socket.set_option(ip::udp::socket::reuse_address(true));
    _socket.bind(listen_endpoint);

    // Join the multicast group.
    _socket.set_option(ip::multicast::join_group(addr));
    _socket.async_receive_from(
        asio::buffer(data_, max_length), _sender_endpoint,
        boost::bind(&receiver::handle_receive_from, this,
          holder::error,
          holder::bytes_transferred));
  }

  void handle_receive_from(const sys::error_code& error, size_t bytes_recvd)
  {
    if (error)
    {
      cout << "connection closed\n";
      return;
    }

    auto count = atoi(data_);
    if (count % 10 == remainder__)
      cout << "my seq: " << count << endl;

    _socket.async_receive_from(
      asio::buffer(data_, max_length), _sender_endpoint,
      boost::bind(&receiver::handle_receive_from, this,
        holder::error,
        holder::bytes_transferred));
  }

private:
  ip::udp::socket _socket;
  ip::udp::endpoint _sender_endpoint;
  enum { max_length = 1024 };
  char data_[max_length];
};

int main(int argc, char* argv[])
{
  try
  {
    auto multicast_address = "239.255.0.1"s;
    if (argc != 2) 
      return EXIT_FAILURE;
      
    remainder__ = atoi(argv[1]);
    cout << "remainder : " << remainder__ << std::endl;

    asio::io_service io_service;
    receiver r(io_service, ip::address::from_string(multicast_address));
    io_service.run();
  }
  catch (exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
