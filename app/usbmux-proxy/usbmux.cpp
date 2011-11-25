#include "usbmux.hpp"
#include <boost/bind.hpp>

using namespace std;
using namespace boost;

namespace usbmux {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
proxy_session::proxy_session(asio::io_service& io_service, uint16_t remote_port)
  : client_socket_(io_service), 
    usbmux_socket_(io_service), usbmux_socket2_(io_service), 
    usbmux_endpoint_("/var/run/usbmuxd"),
    remote_port_(remote_port)
{
}

asio::ip::tcp::socket& proxy_session::socket()
{
  return client_socket_;
}

void proxy_session::start()
{
  system::error_code error = usbmux_socket_.connect(usbmux_endpoint_, error);

  if (!error)
  {
    send_hello();
  } 
  else 
  {
    cerr << "USBMUX Proxy: Could not connect to usbmux" << endl;
  }
}

void proxy_session::send_hello()
{
  usbmux_hello_request* r = reinterpret_cast<usbmux_hello_request*>(buffer_data_);
  r->header.length   = sizeof(usbmux_hello_request);
  r->header.reserved = 0;
  r->header.type     = usbmux_hello;
  r->header.tag      = 2;
  
  asio::async_write (
      usbmux_socket_,
      asio::buffer(buffer_data_, sizeof(usbmux_hello_request)),
      bind(&proxy_session::handle_send_hello, shared_from_this(), asio::placeholders::error)
  );            
}

void proxy_session::handle_send_hello(const system::error_code& error)
{
  if (!error) {
      receive_hello_response();
  } else {
      cerr << "USBMUX Proxy: Could not send hello to usbmux" << endl;
  }
}

void proxy_session::receive_hello_response()
{
  asio::async_read(
      usbmux_socket_,
      asio::buffer(buffer_data_, sizeof(usbmux_response)),
      asio::transfer_at_least(sizeof(usbmux_response)),
      bind(&proxy_session::handle_receive_hello_response, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
      );            
}

void proxy_session::handle_receive_hello_response(const system::error_code& error, size_t bytes_transferred)
{
  if (!error)
      receive_device_id();
  else
      cout << "E: Could not receive hello response from usbmux" << endl;
}

void proxy_session::receive_device_id()
{
  asio::async_read(usbmux_socket_,
      asio::buffer(buffer_data_, 0x011c),
      asio::transfer_at_least(0x011c),
      bind(&proxy_session::handle_receive_device_id, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
      );            
}

void proxy_session::handle_receive_device_id(const system::error_code& error, size_t bytes_transferred)
{
  if (!error)
  {
    device_id_ = buffer_data_[16];
    send_connect();
  } 
  else 
  {
    cout << "E: Could not receive client id from usbmux" << endl;
  }
}

//
void proxy_session::send_connect()
{
  system::error_code error = usbmux_socket2_.connect(usbmux_endpoint_, error);
  if (error)
  {
    cout << "Could not connect to usbmux" << endl;
    return;
  }

  usbmux_connect_request* r = reinterpret_cast<usbmux_connect_request*>(buffer_data_);
  r->header.length   = sizeof(usbmux_connect_request);
  r->header.reserved = 0;
  r->header.type     = usbmux_connect;
  r->header.tag      = 3;
  r->device_id       = device_id_;
  r->dst_port        = htons(remote_port_);
  r->reserved        = 0;

  asio::async_write(
      usbmux_socket2_,
      asio::buffer(buffer_data_, sizeof(usbmux_connect_request)),
      bind(&proxy_session::handle_send_connect, shared_from_this(), asio::placeholders::error)
      );            
}

void proxy_session::handle_send_connect(const system::error_code& error)
{
  receive_connect_response();
}

void proxy_session::receive_connect_response()
{
  asio::async_read(
      usbmux_socket2_,
      asio::buffer(buffer_data_, sizeof(usbmux_response)),
      asio::transfer_at_least(sizeof(usbmux_response)),
      bind(&proxy_session::handle_receive_connect_response, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
      );            
}

void proxy_session::handle_receive_connect_response(const system::error_code& error, size_t bytes_transferred)
{
  if (!error)
  {
    read_from_usbmux();
    read_from_client();
  } 
  else
  {
    cout << "Could not receive connect response usbmux" << endl;
  }
}

void proxy_session::read_from_client()
{
  client_socket_.async_read_some(
      asio::buffer(client_buffer_data_, sizeof(client_buffer_data_)),
      bind(&proxy_session::handle_read_from_client, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
      );
}

void proxy_session::handle_read_from_client(const system::error_code& error, size_t bytes_transferred)
{
  if (!error && bytes_transferred > 0)
  {
    client_buffer_length_ = bytes_transferred;
    write_to_usbmux();
  } 
  else
  {
    cout << "Could not read from client" << endl;
  }
}

void proxy_session::write_to_usbmux()
{
  asio::async_write(
      usbmux_socket2_,
      asio::buffer(client_buffer_data_, client_buffer_length_),
      bind(&proxy_session::handle_write_to_usbmux, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
  );
}

void proxy_session::handle_write_to_usbmux(const system::error_code& error, size_t bytes_transferred)
{
  if (!error)
  {
    read_from_client();
  } 
  else
  {
    cout << "Could not write to usbmux" << endl;
  }
}         

void proxy_session::read_from_usbmux()
{
  usbmux_socket2_.async_read_some(
      asio::buffer(buffer_data_, sizeof(buffer_data_)),
      bind(&proxy_session::handle_read_from_usbmux, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred)
      );
}

void proxy_session::handle_read_from_usbmux(const system::error_code& error, size_t bytes_transferred)
{
  if (!error && bytes_transferred > 0) {
      buffer_length_ = bytes_transferred;
      write_to_client();
  } else {
      cout << "Could not read from usbmux" << endl;
  }
}

void proxy_session::write_to_client()
{
  asio::async_write(
      client_socket_,
      asio::buffer(buffer_data_, buffer_length_),
      bind(&proxy_session::handle_write_to_client, shared_from_this(), asio::placeholders::error)
  );
}

void proxy_session::handle_write_to_client(const system::error_code& error)
{
  if (!error) {
      read_from_usbmux();
  } else {
      cerr << "USBMUX PROXY: Could not write to client" << endl;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
proxy::proxy(asio::io_service& io_service, const asio::ip::tcp::endpoint& endpoint, uint16_t remote_port)
  : io_service_(io_service)
  , acceptor_(io_service, endpoint)
  , remote_port_(remote_port)
{
  proxy_session_ptr new_session(new proxy_session(io_service_, remote_port_));
  acceptor_.async_accept(
      new_session->socket(),
      bind(&proxy::handle_accept, this, new_session, asio::placeholders::error)
  );
}

void proxy::handle_accept(proxy_session_ptr session, const system::error_code& error)
{
  if (error)
  {
    cerr << "USBMUX Proxy: Cannot accept new connection" << endl;
    return;
  }

  session->start();
  proxy_session_ptr new_session(new proxy_session(io_service_, remote_port_));
  acceptor_.async_accept(
      new_session->socket(),
      bind(&proxy::handle_accept, this, new_session, asio::placeholders::error)
      );
}
   
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
