#include <iostream>
#include <sstream>
#include <string>

#include <boost/asio.hpp>
#include "boost/bind.hpp"
#include "boost/date_time/posix_time/posix_time_types.hpp"

namespace asio = boost::asio;
namespace ip   = boost::asio::ip;

const short multicast_port  = 30001;
int const max_message_count = 10000000;
int const INTERVAL = 1;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class sender
{
public:
  sender(asio::io_service& io_service, ip::address const& multicast_address)
    : _endpoint(multicast_address, multicast_port)
    , _socket(io_service, _endpoint.protocol())
    , _timer(io_service)
    , _message_count(0)
  {
    std::ostringstream os;
    os << "Message " << _message_count++;
    message_ = os.str();

    ip::multicast::hops ttl(4);
    _socket.set_option(ttl);
    _socket.async_send_to(asio::buffer(message_), _endpoint,
      boost::bind(&sender::handle_send_to, this, asio::placeholders::error));
  }

  void handle_send_to(const boost::system::error_code& error)
  {
    if (!error && _message_count < max_message_count)
    {
      _timer.expires_from_now(boost::posix_time::milliseconds(INTERVAL));
      _timer.async_wait(
        boost::bind(&sender::handle_timeout, this, asio::placeholders::error));
    }
  }

  void handle_timeout(const boost::system::error_code& error)
  {
    if (error) return;

    std::ostringstream os;
    os << _message_count++;
    message_ = os.str();

    _socket.async_send_to(
      asio::buffer(message_), _endpoint,
      boost::bind(&sender::handle_send_to, this, asio::placeholders::error));
  }

private:
  ip::udp::endpoint _endpoint;
  ip::udp::socket _socket;
  asio::deadline_timer _timer;
  int _message_count;
  std::string message_;
};

int main(int argc, char* argv[])
{
  try
  {
    std::string addr = "239.255.0.1";
    if (argc == 2) addr = argv[1];

    asio::io_service io_service;
    sender s(io_service, asio::ip::address::from_string(addr));
    io_service.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
