#include <iostream>
#include <sstream>
#include <string>

#include <boost/asio.hpp>
#include "boost/bind.hpp"
#include "boost/date_time/posix_time/posix_time_types.hpp"

namespace asio = boost::asio;
namespace ip   = boost::asio::ip;

const short multicast_port    = 30001;
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
    : endpoint_(multicast_address, multicast_port)
    , socket_(io_service, endpoint_.protocol())
    , timer_(io_service)
    , message_count_(0)
  {
    std::ostringstream os;
    os << "Message " << message_count_++;
    message_ = os.str();

    ip::multicast::hops ttl(4);
    socket_.set_option(ttl);
    socket_.async_send_to(asio::buffer(message_), endpoint_,
      boost::bind(&sender::handle_send_to, this, asio::placeholders::error));
  }

  void handle_send_to(const boost::system::error_code& error)
  {
    if (!error && message_count_ < max_message_count)
    {
      timer_.expires_from_now(boost::posix_time::milliseconds(INTERVAL));
      timer_.async_wait(
        boost::bind(&sender::handle_timeout, this, asio::placeholders::error));
    }
  }

  void handle_timeout(const boost::system::error_code& error)
  {
    if (error) return;

    std::ostringstream os;
    os << message_count_++;
    message_ = os.str();

    socket_.async_send_to(
      asio::buffer(message_), endpoint_,
      boost::bind(&sender::handle_send_to, this, asio::placeholders::error));
  }

private:
  ip::udp::endpoint endpoint_;
  ip::udp::socket socket_;
  asio::deadline_timer timer_;
  int message_count_;
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
