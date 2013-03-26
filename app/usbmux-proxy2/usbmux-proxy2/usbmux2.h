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
enum
{
    usbmux_result  = 1,
    usbmux_connect = 2,
    usbmux_hello   = 3,     // in python hello is LISTEN
};

struct usbmux_header
{
    uint32_t length;
    uint32_t version;
    uint32_t type;
    uint32_t tag;
};

struct usbmux_response
{
    usbmux_header header;
    uint32_t result;
};

struct usbmux_hello_request
{
    usbmux_header header;
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
    void handle_write_to_usbmux(system::error_code const& error, size_t bytes_transffered);

    // relay: usbmux
    void read_from_usbmux();
    void handle_read_from_usbmux(system::error_code const& error, size_t bytes_transffered);
    void write_to_client();
    void handle_write_to_client(system::error_code const& error, size_t bytes_transffered);

private:
    tcp::socket _client_socket;

    asio::local::stream_protocol::socket _usbmux_socket;
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
