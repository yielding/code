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
    : socket_(io_service)
  {
    ip::udp::endpoint listen_endpoint(ip::address::from_string("0.0.0.0"), multicast_port);
    socket_.open(listen_endpoint.protocol());
    socket_.set_option(ip::udp::socket::reuse_address(true));
    socket_.bind(listen_endpoint);

    // Join the multicast group.
    socket_.set_option(ip::multicast::join_group(addr));
    socket_.async_receive_from(
        asio::buffer(data_, max_length), sender_endpoint_,
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

    socket_.async_receive_from(
      asio::buffer(data_, max_length), sender_endpoint_,
      boost::bind(&receiver::handle_receive_from, this,
        holder::error,
        holder::bytes_transferred));
  }

private:
  ip::udp::socket socket_;
  ip::udp::endpoint sender_endpoint_;
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
