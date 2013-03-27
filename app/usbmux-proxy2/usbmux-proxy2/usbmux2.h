#ifndef USBMUX_PROXY2__SESSION_H__
#define USBMUX_PROXY2__SESSION_H__

#include <boost/asio.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/shared_ptr.hpp>

#include <stdint.h>

using boost::asio::ip::tcp;
using namespace boost;
using namespace std;

namespace usbmux2 {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct usbmux_header
{
  uint32_t length;
  uint32_t version;
  uint32_t type;
  uint32_t tag;

  std::string to_s()
  {
    char buf[512] = { 0 };
    sprintf(buf, "[%d, %d, %d, %d]", length, version, type, tag);

    return string(buf);
  }
};

int const SZ = 32 * 1024;

class ProxySession: public boost::enable_shared_from_this<ProxySession>
{
public:
  ProxySession(asio::io_service& ios, uint16_t rport);

public:
  tcp::socket& socket();

public:
  void start();

  // hello
  void send_hello();
  void handle_send_hello(system::error_code const& error);
  void receive_hello_response();
  void handle_receive_hello_response(system::error_code const& error, size_t bytes_transffered);

  void receive_device_id();
  void handle_receive_device_id(system::error_code const& error, size_t bytes_transffered);

  // connect
  void send_connect();
  void handle_send_connect(system::error_code const& error);
  void receive_connect_response();
  void handle_receive_connect_response(system::error_code const& error, size_t bytes_transffered);

  // relay: client
  void read_from_client();
  void handle_read_from_client(system::error_code const& error, size_t bytes_transffered);
  void write_to_usbmux();
  void handle_write_to_usbmux(system::error_code const& error);

  // relay: usbmux
  void read_from_usbmux();
  void handle_read_from_usbmux(system::error_code const& error, size_t bytes_transffered);
  void write_to_client();
  void handle_write_to_client(system::error_code const& error);

private:
  void clear_buffer();

private:
  tcp::socket _client_socket;

  // REMARK
  // what if it's not a unix socket but TCP/IP module?
  asio::local::stream_protocol::socket _usbmux_socket, _usbmux_socket2;
  asio::local::stream_protocol::endpoint _usbmux_endpoint;

  uint16_t _remote_port;

private:
  char   _mux_buffer_data[SZ];
  size_t _mux_buffer_length;

  char   _cli_buffer_data[SZ];
  size_t _cli_buffer_length;

  int _device_id;
  uint32_t _tag;
};

typedef boost::shared_ptr<ProxySession> ProxySessionPtr;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Proxy
{
public:
  Proxy(asio::io_service& ios, tcp::endpoint const endpoint, uint16_t rport);

public:
  void handle_accept(ProxySessionPtr, system::error_code const& error);

private:
  asio::io_service& _ios;
  tcp::acceptor _acceptor;
  uint16_t _rport;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}

#endif
