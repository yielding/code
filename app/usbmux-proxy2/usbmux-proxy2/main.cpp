#include "usbmux2.h"
#include <iostream>

int main(int argc, const char * argv[])
{
  try
  {
    asio::io_service ios;
    tcp::endpoint endpoint(tcp::v4(), 2222);
    usbmux2::RelayServer server(ios, endpoint, 22);
    ios.run();
  }
  catch(std::exception& e)
  {
    cout << "Can't bind the given port!" << endl;
  }

  return 0;
}
