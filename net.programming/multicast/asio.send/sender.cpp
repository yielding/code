#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <chrono>

#include <boost/asio.hpp>

namespace asio = boost::asio;
namespace ip   = boost::asio::ip;

using namespace std;

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
  sender(asio::io_context& io_context, ip::address const& multicast_address)
    : _endpoint(multicast_address, multicast_port)
    , _socket(io_context, _endpoint.protocol())
    , _timer(io_context)
    , _message_count(0)
  {
    ostringstream os;
    os << "Message " << _message_count++;
    _message = os.str();

    ip::multicast::hops ttl(4);
    _socket.set_option(ttl);
    _socket.async_send_to(asio::buffer(_message), _endpoint,
      bind_front(&sender::handle_send_to, this));
  }

  void handle_send_to(const boost::system::error_code& error, size_t = 0)
  {
    if (!error && _message_count < max_message_count)
    {
      _timer.expires_after(chrono::milliseconds(INTERVAL));
      _timer.async_wait(bind_front(&sender::handle_timeout, this));
    }
  }

  void handle_timeout(const boost::system::error_code& error)
  {
    if (error) return;

    ostringstream os;
    os << _message_count++;
    _message = os.str();

    _socket.async_send_to(asio::buffer(_message), _endpoint,
      bind_front(&sender::handle_send_to, this));
  }

private:
  ip::udp::endpoint _endpoint;
  ip::udp::socket _socket;
  asio::steady_timer _timer;
  int _message_count;
  string _message;
};

int main(int argc, char* argv[])
{
  try
  {
    auto addr = "239.255.0.1"s;
    if (argc == 2) addr = argv[1];

    asio::io_context io_context;
    sender s(io_context, asio::ip::make_address(addr));
    io_context.run();
  }
  catch (exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
