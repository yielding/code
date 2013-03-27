#include "stdafx.h"
#include "usbmux2.h"

#include "BPlist.h"
#include "PlistParser.h"
#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>
#include <iostream>
#include <map>

using namespace utility::parser;

namespace usbmux2 {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void ps(usbmux_header* h, string s)
{
  cout << "[send packet] :" << h->to_s() << endl << s << endl;
}

void pr(usbmux_header* h, string s)
{
  cout << "[recv packet] :" << h->to_s() << endl << s << endl;
}

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
  , _usbmux_socket(ios), _usbmux_socket2(ios)
  , _usbmux_endpoint("/var/run/usbmuxd")
  , _remote_port(rport)
  , _tag(0)
{
}

tcp::socket& ProxySession::socket()
{
  return _client_socket;
}

void ProxySession::start()
{
  system::error_code error; _usbmux_socket.connect(_usbmux_endpoint, error);

  if (error)
  {
    cerr << "USBMUX Proxy2: Could not connect to usbmux" << endl;
    return;
  }

  send_hello();
}

void ProxySession::send_hello()
{
  CFDictionary dict;
  dict.add("ClientVersionString", "usbmux2 by yielding");
  dict.add("MessageType", "Listen");
  dict.add("ProgName",    "tcprelay");
  PropertyList plist;
  auto s = plist.set(dict).to_xml();
  
  _tag++;
  auto header = reinterpret_cast<usbmux_header*>(_mux_buffer_data);
  header->length  = sizeof(usbmux_header) + uint32_t(s.length());
  header->version = 1;             // support only version 1
  header->type    = 8;             // 8 means TYPE_PLIST
  header->tag     = _tag;  
  strncpy(_mux_buffer_data + 16, s.c_str(), s.length());
  
  ps(header, s);
  
  asio::async_write(_usbmux_socket,
      asio::buffer(_mux_buffer_data, header->length),
      bind(&ProxySession::handle_send_hello, shared_from_this(),
            asio::placeholders::error));
}

void ProxySession::handle_send_hello(const system::error_code& error)
{
  if (error)
  {
    cout << "E: Could not receive hello response from usbmux" << endl;
    return;
  }

  receive_hello_response();
}

void ProxySession::receive_hello_response()
{
  asio::read(_usbmux_socket, asio::buffer(_mux_buffer_data, 4));
  auto length = *(uint32_t *)_mux_buffer_data;
  
  asio::async_read(_usbmux_socket,
      asio::buffer(_mux_buffer_data + 4, length - 4),
      asio::transfer_at_least(length - 4),
      bind(&ProxySession::handle_receive_hello_response, shared_from_this(), 
           asio::placeholders::error,
           asio::placeholders::bytes_transferred));    
}

void ProxySession::handle_receive_hello_response(system::error_code const& error, size_t bytes_transffered)
{
  if (!error)
  {
    auto  header = reinterpret_cast<usbmux_header*>(_mux_buffer_data);
    auto  length = header->length;
    auto version = header->version;
    auto     tag = header->tag;

    if (version != 1) 
      throw runtime_error("version mismatch");

    if (tag != _tag)
      throw runtime_error("tag mismatch");

    pr(header, string(_mux_buffer_data + 16, length - 16));

    PListParser parser;
    parser.init_with_string(string(_mux_buffer_data + 16, length - 16));
    auto value = parser.get_string("Number", "plist.dict");
    if (value != "0")
      throw runtime_error("wrong hello response");

    receive_device_id();
  }
  else
  {
    cerr << "USBMUX2 Proxy: Could not send hello to usbmux" << endl;
  }
}

void ProxySession::receive_device_id()
{
  asio::read(_usbmux_socket, asio::buffer(_mux_buffer_data, 4));
  auto length = *(uint32_t *)_mux_buffer_data;
  
  asio::async_read(_usbmux_socket,
      asio::buffer(_mux_buffer_data + 4, length - 4),
      asio::transfer_at_least(length - 4),
      bind(&ProxySession::handle_receive_device_id, shared_from_this(),
           asio::placeholders::error,
           asio::placeholders::bytes_transferred));
}

void ProxySession::handle_receive_device_id(system::error_code const& error, size_t bytes_transffered)
{
  if (!error)
  {
    auto header = reinterpret_cast<usbmux_header*>(_mux_buffer_data);
    auto length = header->length;

    pr(header, string(_mux_buffer_data + 16, length - 16));
    
    PListParser parser;
    parser.init_with_string(string(_mux_buffer_data + 16, length - 16));
    auto msg_type = parser.get_string("MessageType", "plist.dict");
    if (msg_type != "Attached")
    {
      cerr << "No available device information" << endl;
      return;
    }

    auto dict  = parser.get_dict("Properties", "plist.dict");
    _device_id = lexical_cast<int>(dict["DeviceID"]);
    auto location_id = dict["LocationID"];
    auto product_id  = dict["ProductID"];

    _tag = 0;
    
    send_connect();
  } 
  else 
  {
    cout << "E: Could not receive client id from usbmux" << endl;
  }
}

void ProxySession::send_connect()
{
  clear_buffer();
  system::error_code error; _usbmux_socket2.connect(_usbmux_endpoint, error);

  CFDictionary dict;
  dict.add("ClientVersionString", "usbmux2 by yielding");
  dict.add("DeviceID", CFInteger(_device_id));
  dict.add("MessageType", "Connect");
  dict.add("PortNumber", CFInteger(htons(_remote_port)));
  dict.add("ProgName", "tcprelay");
  PropertyList plist;
  auto s = plist.set(dict).to_xml();
  
  _tag++;
  auto header = reinterpret_cast<usbmux_header*>(_mux_buffer_data);
  header->length  = sizeof(usbmux_header) + uint32_t(s.length());
  header->version = 1;
  header->type    = 8;             // 8 means TYPE_PLIST
  header->tag     = _tag;
  strncpy(_mux_buffer_data + 16, s.c_str(), s.length());
  
  ps(header, s);
  
  asio::async_write(_usbmux_socket2,
    asio::buffer(_mux_buffer_data, header->length),
    bind(&ProxySession::handle_send_connect, shared_from_this(),
         asio::placeholders::error));
}

void ProxySession::handle_send_connect(system::error_code const& error)
{
  if (error)
  {
    cerr << "Error: connection to usbmuxd have failed!\n";
    return;
  }
  
  receive_connect_response();
}

void ProxySession::receive_connect_response()
{
  clear_buffer();
  asio::read(_usbmux_socket2, asio::buffer(_mux_buffer_data, 4));
  auto length = *(uint32_t *)_mux_buffer_data;
   
  asio::async_read(_usbmux_socket2,
      asio::buffer(_mux_buffer_data + 4, length - 4),
      asio::transfer_at_least(length - 4),
      bind(&ProxySession::handle_receive_connect_response, shared_from_this(),
           asio::placeholders::error,
           asio::placeholders::bytes_transferred));
}

void ProxySession::handle_receive_connect_response(system::error_code const& error, size_t bytes_transffered)
{
  if (error)
  {
    cout << "Could not receive connect response usbmux\n";
    return;
  }

  auto header = reinterpret_cast<usbmux_header*>(_mux_buffer_data);
  if (header->tag != _tag)
    throw runtime_error("Tag mismatch! cannot start new session!\n");

  auto length = header->length;
  pr(header, string(_mux_buffer_data + 16, length - 16));

  PListParser parser;
  parser.init_with_string(string(_mux_buffer_data + 16, length - 16));
  auto value = parser.get_string("Number", "plist.dict");
  if (value != "0")
    throw runtime_error("wrong hello response");

  read_from_usbmux();
  read_from_client();
}

//
// TODO here
// why async_read_some fails here?
//   1. invalid _client_socket
//   2. async_read_some
//
void ProxySession::read_from_client()
{
  _client_socket.async_read_some(
      asio::buffer(_cli_buffer_data, sizeof(_cli_buffer_data)),
      bind(&ProxySession::handle_read_from_client, shared_from_this(),
           asio::placeholders::error,
           asio::placeholders::bytes_transferred));
}

void ProxySession::handle_read_from_client(system::error_code const& error, size_t bytes_transffered)
{
  if (!error && bytes_transffered > 0)
  {
    _cli_buffer_length = bytes_transffered;
    write_to_usbmux();
  }
  else
  {
    cout << "Could not read from client" << endl;
  }
}

void ProxySession::write_to_usbmux()
{
  asio::async_write(
    _usbmux_socket2,
    asio::buffer(_cli_buffer_data, _cli_buffer_length),
    bind(&ProxySession::handle_write_to_usbmux, shared_from_this(), 
         asio::placeholders::error));
}

void ProxySession::handle_write_to_usbmux(system::error_code const& error)
{
  if (error)
  {
    cout << "Could not write to usbmux" << endl;
    return;
  }

  read_from_client();
}

void ProxySession::read_from_usbmux()
{
  _usbmux_socket2.async_read_some(
      asio::buffer(_mux_buffer_data, sizeof(_mux_buffer_data)),
      bind(&ProxySession::handle_read_from_usbmux, shared_from_this(),
           asio::placeholders::error,
           asio::placeholders::bytes_transferred));
}

void ProxySession::handle_read_from_usbmux(system::error_code const& error, size_t bytes_transffered)
{
  if (!error && bytes_transffered > 0)
  {
    _mux_buffer_length = bytes_transffered;
    write_to_client();
  }
  else
  {
    cout << "Could not read from usbmux" << endl;
  }
}

void ProxySession::write_to_client()
{
  asio::async_write(
    _client_socket,
    asio::buffer(_mux_buffer_data, _mux_buffer_length),
    bind(&ProxySession::handle_write_to_client, shared_from_this(),
         asio::placeholders::error));
}

void ProxySession::handle_write_to_client(system::error_code const& error)
{
  if (error)
  {
    cerr << "USBMUX could not write to client" << endl;
    return;
  }

  read_from_usbmux();
}

////////////////////////////////////////////////////////////////////////////////
//
// Proxy 이름 다시 만든다.
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
      bind(&Proxy::handle_accept, this, new_session, 
           asio::placeholders::error));
}

void ProxySession::clear_buffer()
{
  memset(_mux_buffer_data, 0x00, sizeof(_mux_buffer_data));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
