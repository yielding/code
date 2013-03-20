#include "ByteBuffer.h"

#include <boost/asio.hpp>
#include <string>
#include <map>

using boost::asio::ip::tcp;
using utility::hex::ByteBuffer;

////////////////////////////////////////////////////////////////////////////////
//
// SafeStreamSocket is already implemented in ASIO
//
////////////////////////////////////////////////////////////////////////////////
class MuxDevice
{
public:
  MuxDevice(int device_id, int product_id, int serial_no, int location_id)
  {
    _device_id   = device_id;
    _product_id  = product_id;
    _serial_no   = serial_no;
    _location_id = location_id;
  }

private:
  int _device_id;
  int _product_id;
  int _serial_no;
  int _location_id;
};

////////////////////////////////////////////////////////////////////////////////
//
// REMARK packet type을 만드는 것보다 Byte Buffer를 만드는 것이 유연하다.
// 타입을 하나라도 적게 만드는 것이..
//
////////////////////////////////////////////////////////////////////////////////
/*
struct usbmux_header
{
    uint32_t length;
    uint32_t version;
    uint32_t type;          // request, or response
    uint32_t tag;
};
*/

class BinaryProtocol
{
public:
    enum { VERSION = 0, 
           TYPE_RESULT, TYPE_CONNECT, TYPE_LISTEN,
           TYPE_DEVICE_ADD, TYPE_DEVICE_REMOVE };
public:
    BinaryProtocol(tcp::socket& s);

    void send_packet(uint32_t req, uint32_t tag, ByteBuffer& payload);
    void recv_packet();

private:
    auto pack(uint32_t req, std::map<std::string, int>& payload) -> ByteBuffer;
    auto unpack(uint32_t resp, ByteBuffer const& payload) -> std::map<std::string, int>;

protected:
    tcp::socket& _socket;
};

class PlistProtocol: public BinaryProtocol
{
public:
    PlistProtocol(tcp::socket& s);
};

////////////////////////////////////////////////////////////////////////////////
//
// MuxConnection은 여러 가지 Protocol 중에서 하나를 선택하는 것이므로
// Template 인자로서의 Protocol은 적당하지 않다. 상속이나 독립 클래스가 적당하지
//
////////////////////////////////////////////////////////////////////////////////
class MuxConnection
{
public:
    MuxConnection(std::string const& socket_path);

public:
    void listen();
    void process(int timeout);
    void connect(MuxDevice device, int port);
    void close();

private:
    void get_replay();
    void process_packet();
    void exchange();

private:
    std::string _socket_path;
};

////////////////////////////////////////////////////////////////////////////////
//
// A client communicating USBMUXd
//
////////////////////////////////////////////////////////////////////////////////
class USBMux 
{
public:
    USBMux(std::string const& socket_path="");
    ~USBMux();

    void connect()
    {
    }

    void process()
    {
    }

private:
    std::string _socket_path;
    MuxConnection* _listener;
    int _version;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
