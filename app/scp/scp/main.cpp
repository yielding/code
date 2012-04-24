#include "SSHSession.h"
#include "SCPChannel.h"

#include <iostream>
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
    vector<pair<string, string>> hosts;
    hosts.push_back(make_pair("127.0.0.1",      "/Users/yielding/download_test"));
    hosts.push_back(make_pair("219.241.220.98", "/home/yielding/test"));
       
    int index = 1;
    if (argc > 1) 
        index = atoi(argv[1]);
    
    auto host = hosts[index].first;
    auto from = hosts[index].second;
    auto to   = "/Users/yielding/down";
    
    utility::comm::ssh::SSHSession ssh(host);
    if (!ssh.connect("yielding", ""))
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
