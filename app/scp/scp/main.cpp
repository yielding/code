#include "SSHSession.h"
#include "SCPChannel.h"

#include <iostream>
#include <boost/format.hpp>

using namespace std;
using namespace boost;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
    utility::comm::ssh::SSHSession ssh("127.0.0.1");

    if (!ssh.connect("yielding", "alsrudk!"))
    {
        cout << str(format("ssh.connect error: %s\n") % ssh.error_msg());
        return EXIT_FAILURE;
    }
    
    auto scp = ssh.create_scp_channel();
    scp->download("/Users/yielding/download_test/*", "/Users/yielding/down");

    cout << "ssh.connect ok\n";

    return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////