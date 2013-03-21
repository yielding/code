#include "usbmux2.h"
#include <boost/bind.hpp>
#include <iostream>

namespace usbmux2 {
////////////////////////////////////////////////////////////////////////////////
//
// rport: remote port represents iphone
//
// 1차 구현은 UNIX domain socket으로 한다. 
// Why? asio를 쓰면 바꾸는 것은 문제가 안되므로
// 
////////////////////////////////////////////////////////////////////////////////
ProxySession::ProxySession(asio::io_service& ios, uint16_t rport)
  : _client_socket(ios)
  , _usbmux_socket(ios)
  , _usbmux_endpoint("/var/run/usbmuxd")
  , _remote_port(rport)
{
}

tcp::socket& ProxySession::socket()
{
  return _client_socket;
}

void ProxySession::start()
{
  system::error_code error;
  _usbmux_socket.connect(_usbmux_endpoint, error);

  if (!error)
    send_hello();
  else
    cerr << "USBMUX Proxy2: Could not connect to usbmux" << endl;
}

void ProxySession::send_hello()
{
  auto r = reinterpret_cast<usbmux_hello_request*>(_mux_buffer_data);
  r->header.length  = sizeof(usbmux_hello_request);
  r->header.version = 0;
  r->header.type    = usbmux_hello;
  r->header.tag     = 2;

  asio::async_write(_usbmux_socket,
      asio::buffer(_mux_buffer_data, sizeof(usbmux_hello_request)),
      bind(&ProxySession::handle_send_hello,
            shared_from_this(), 
            asio::placeholders::error));       
}

void ProxySession::handle_send_hello(const system::error_code& error)
{
  if (!error)
  {
    receive_hello_response();
  }
  else
  {
    cout << "E: Could not receive hello response from usbmux" << endl;
  }
}

void ProxySession::receive_hello_response()
{
  asio::async_read(_usbmux_socket,
      asio::buffer(_mux_buffer_data, sizeof(usbmux_response)),
      asio::transfer_at_least(sizeof(usbmux_response)),
      bind(&ProxySession::handle_receive_hello_response, shared_from_this(), asio::placeholders::error,
        asio::placeholders::bytes_transferred));    
}

void ProxySession::handle_receive_hello_response(system::error_code const& error, size_t bytes_transffered)
{
}

void ProxySession::receive_device_id()
{
}

void ProxySession::handle_receive_device_id()
{
}

void ProxySession::send_connect()
{
}

void ProxySession::handle_send_connect(system::error_code const& error)
{
}

void ProxySession::receive_connect_response()
{
}

void ProxySession::handle_receive_connect_response(system::error_code const& error, size_t bytes_transffered)
{
}

void ProxySession::read_from_client()
{
}

void ProxySession::handle_read_from_client(system::error_code const& error, size_t bytes_transffered)
{
}

void ProxySession::write_to_usbmux()
{
}

void ProxySession::handle_write_to_usbmux(system::error_code const& error, size_t bytes_transffered)
{
}

void ProxySession::read_from_usbmux()
{
}

void ProxySession::handle_read_from_usbmux(system::error_code const& error, size_t bytes_transffered)
{
}

void ProxySession::write_to_client()
{
}

void ProxySession::handle_write_to_client(system::error_code const& error, size_t bytes_transffered)
{
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Proxy::Proxy(asio::io_service& ios, tcp::endpoint const endpoint, uint16_t rport)
  : _ios(ios)
  , _acceptor(ios, endpoint)
  , _rport(rport)
{
  ProxySessionPtr new_session(new ProxySession(_ios, _rport));
  _acceptor.async_accept(new_session->socket(),
      bind(&Proxy::handle_accept, this, new_session, asio::placeholders::error));
}

void Proxy::handle_accept(ProxySessionPtr session, system::error_code const& error)
{
  if (error)
  {
    cerr << "USBMUX Proxy: Cannot accept new connection" << endl;
    return;
  }

  session->start();

  ProxySessionPtr new_session(new ProxySession(_ios, _rport));
  _acceptor.async_accept(new_session->socket(),
      bind(&Proxy::handle_accept, this, new_session, asio::placeholders::error));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
