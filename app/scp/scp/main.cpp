#include "SSHSession.h"
#include "SFTPChannel.h"

// #include "SCPChannel.h"
// #include "SSHHostList.h"

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
    /*
    SSHHostList hosts;
    if (!hosts.init_with_file("/Users/yielding/.passwd"))
        return EXIT_FAILURE;
    
    string host, user, pw, from, xx;
    int port;
    tie(host, port, user, pw, from) = hosts.nth(1);
    */

    string host = "127.0.0.1";
    int    port = 2222;
    string user = "root";
    string pw   = "alpine";
    string from = "/mnt2/mobile";
    string to   = "/Users/yielding/down";
    
    utility::comm::ssh::SSHSession ssh(host, user, port);
    if (!ssh.connect(pw))
    {
        cout << str(format("ssh.connect error: %s\n") % ssh.error_msg());
        return EXIT_FAILURE;
    }
    
    try 
    {
        auto sftp = ssh.create_sftp_channel();

        vector<string> files;
        auto p = [&files](string const& name, uint64_t size, bool is_dir, uint64_t ct, uint32_t mt, uint32_t at) {
            if (!is_dir)
                files.push_back(name);
            
            cout << str(format("%s : %ld\n") % name % size) << endl;
        };

        auto res = sftp->scan_dir(from, p);
        if (!res)
        {
            cout << "ssh.download error\n";
            return EXIT_FAILURE;
        }
        
        ofstream ofs("/Users/yielding/Desktop/all.bin");
        
        auto mdf_writer = [&ofs](char* buffer, int size) {
            ofs.write(buffer, size);
        };
         
        for (auto it = files.begin(); it != files.end(); ++it)
        {
            cout << *it << endl;
            sftp->download_to(*it, mdf_writer);
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
