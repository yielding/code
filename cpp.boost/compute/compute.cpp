#include <iostream>

#include <boost/compute/core.hpp>

namespace compute = boost::compute;

int main()
{
    // get the default device
    for (auto device : compute::system::devices())
    {
        // print the device's name
        std::cout << "hello from " << device.name() << std::endl;

    }

    return 0;
}
