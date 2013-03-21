#include "usbmux2.h"
#include <iostream>

int main(int argc, const char * argv[])
{
    asio::io_service ios;
    tcp::endpoint endpoint(tcp::v4(), 2222);
    usbmux2::Proxy proxy(ios, endpoint, 22);
    ios.run();

    return 0;
}
