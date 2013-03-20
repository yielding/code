#ifndef USBMUX_H
#define USBMUX_H

#include "stdafx.h"
#include "usbmux.h"

using namespace std;
using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
BinaryProtocol::BinaryProtocol(tcp::socket& s)
    : _socket(s)
{
}

void BinaryProtocol::send_packet(uint32_t reqest, uint32_t tag, ByteBuffer& payload)
{
}

auto BinaryProtocol::pack(uint32_t req, map<string, int>& payload) 
     -> ByteBuffer
{
    ByteBuffer packet;
    if (req == TYPE_CONNECT)
    {
        uint32_t device_id = payload["DeviceID"];
        uint16_t port_no   = payload["PortNumber"];

        packet.set_uint4_le(device_id).set_uint2_le(port_no)
              .set_uint1(0).set_uint1(0);

        return packet;
    }
    else if (req == TYPE_LISTEN)
    {
        return packet;
    }

    throw std::runtime_error("Invalid outgoint request");
}

auto BinaryProtocol::unpack(uint32_t resp, ByteBuffer const& payload) 
    -> map<string, int>
{
    map<string, int> result;

    if (resp == TYPE_RESULT)
    {
        uint32_t no = payload.get_uint4_le();
        result["Number"] = no;
        return result;
    }

    if (resp == TYPE_DEVICE_ADD)
    {
        uint32_t device_id;
        uint16_t usbpid;
        string   serial;
        uint16_t padding;
        uint32_t location;

        result["DeviceID"]     = device_id;
        result["LocationID"]   = location;
        // TODO
        // result["SerialNumber"] = serial;
        result["ProductID"]    = usbpid;

        return result;
    }

    if (resp == TYPE_DEVICE_REMOVE)
    {
        result["DeviceID"] = payload.get_uint4_le();
        return result;
    }

    throw std::runtime_error("Invalid incoming response type");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
PlistProtocol::PlistProtocol(tcp::socket& s)
    : BinaryProtocol(s)
{
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
MuxConnection::MuxConnection(std::string const& socket_path)
    : _socket_path(socket_path)
{
    if (1) 
    {
    }
}

void MuxConnection::listen()
{
}

void MuxConnection::process(int timeout)
{
}

void MuxConnection::connect(MuxDevice device, int port)
{
}

void MuxConnection::close()
{
}

void MuxConnection::get_replay()
{
}

void MuxConnection::process_packet()
{
}

void MuxConnection::exchange()
{
}

////////////////////////////////////////////////////////////////////////////////
//
//  REMARK!!
//    원격지에 있는 컴퓨터와 통신이 가능한지 확인해본다.
//
////////////////////////////////////////////////////////////////////////////////
USBMux::USBMux(string const& socket_path)
{
    _version = 0;

    _socket_path = socket_path;
    _listener    = new MuxConnection(_socket_path);
}

USBMux::~USBMux()
{
    if (_listener)
    {
        delete _listener;
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
