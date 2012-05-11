#include "SSHSession.h"
#include "SCPChannel.h"
#include "SSHHostList.h"

#include <iostream>
#include <fstream>
#include <vector>
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
    SSHHostList hosts;
    
    if (!hosts.init_with_file("/Users/yielding/.passwd"))
        return EXIT_FAILURE;
    
    string host, user, pw, from, xx;
    int port;
    tie(host, port, user, pw, from) = hosts.nth(1);
    
    string to = "/Users/yielding/down";
    
    utility::comm::ssh::SSHSession ssh(host, user, port);
    if (!ssh.connect(pw))
    {
        cout << str(format("ssh.connect error: %s\n") % ssh.error_msg());
        return EXIT_FAILURE;
    }
    
    try 
    {
        auto scp = ssh.create_scp_channel();
        auto res = scp->download(from, to);
        if (!res)
        {
            cout << "ssh.download error\n";
            return EXIT_FAILURE;
        }
    } 
    catch (std::exception& e) 
    {
        cout << str(format("exception occurred: %s\n") % e.what());
    }

    cout << "ssh.download complete ok\n";

    return EXIT_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
