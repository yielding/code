#include <boost/asio.hpp>

#include <cstdlib>
#include <iostream>

#include "usbmux.hpp"

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool parse_port(string const& arg, uint16_t& port, uint16_t& local_port)
{
  if (arg.find(':'))
  {
    string a = arg.substr(0, arg.find(':'));
    string b = arg.substr(arg.find(':') + 1);
    port = atoi(a.c_str());
    local_port = atoi(b.c_str());
  } 
  else
  {
    port = local_port = atoi(arg.c_str());
  }

  return (port != 0) && (local_port != 0);
}

int main(int argc, char** argv)
{
  if (argc < 2)
  {
    cerr << "Usage: usbmux-proxy <port>[:local-port] [<port>[:local-port] ...]\n";
    return 1;
  }

  try
  {
    // Check port specifications
    for (int i=1; i<argc; ++i)
    {
      uint16_t port, local_port;
      if (!parse_port(argv[i], port, local_port))
      {
        cerr << "Invalid port specification: " << argv[1] << endl;
        return 1;
      }
    }      

    // Start the proxies
    asio::io_service io_service;
    usbmux::proxy_list proxies;

    for (int i=1; i<argc; ++i)
    {
      uint16_t port, local_port;
      if (parse_port(argv[i], port, local_port))
      {
        cout << "Creating usbmux proxy for port " << local_port 
             << " (local) to " << port 
             << " (iphone)" << endl;
        asio::ip::tcp::endpoint endpoint(asio::ip::tcp::v4(), local_port);
        usbmux::proxy_ptr proxy(new usbmux::proxy(io_service, endpoint, port));
        proxies.push_back(proxy);
      }
    }

    io_service.run();
  }
  catch (std::exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
