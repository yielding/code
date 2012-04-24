#include "stdafx.h"
#include "SCPChannel.h"
#include "SSHSession.h"
#include "ScopeGuard.h"

#include <string>
#include <fstream>
#include <stdexcept>

#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/phoenix.hpp>
#include <boost/filesystem.hpp>

using namespace std;
using namespace boost;
using boost::phoenix::arg_names::arg1;

namespace phx = boost::phoenix;
namespace  fs = boost::filesystem;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
    string create_path(string dest, vector<string> dirs)
    {
        auto path = ends_with(dest, "/") 
             ? dest.substr(0, dest.length()-1) 
             : dest;
        
        for_each(dirs.begin(), dirs.end(), phx::ref(path) += ("/" + arg1));

#ifdef WIN32
        replace_all(path, "/", "\\");
#endif
        if (!fs::exists(path))
            fs::create_directory(path);
        
        return path;
    }
}

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SCPChannel::SCPChannel(SSHSession* session)
    : _ssh(session)
{
}

SCPChannel::~SCPChannel()
{
}

bool SCPChannel::download(string const& from, string const& to)
{
    _scp = ::ssh_scp_new(_ssh->session(), 
                         SSH_SCP_READ | SSH_SCP_RECURSIVE, from.c_str());
    if (_scp == NULL)
        throw std::runtime_error("SCP channel creation error");
    
    ON_BLOCK_EXIT(phx::bind(&ssh_scp_free, phx::ref(_scp)));
    
    if (::ssh_scp_init(_scp) != SSH_OK)
    {
        _ssh->error("scp.download: ");
        return false;
    }

    vector<string> dir_components; 
    do 
    {
        int rc = ::ssh_scp_pull_request(_scp);
        if (rc == SSH_ERROR)
        {
            ::ssh_scp_close(_scp);
            return dir_components.empty();
        }

        if (rc == SSH_SCP_REQUEST_NEWFILE)
        {
            auto size = ::ssh_scp_request_get_size(_scp);
            auto mode = ::ssh_scp_request_get_permissions(_scp);
            auto path = create_path(to, dir_components);
                 path = str(format("%s/%s") 
                            % path 
                            % string(::ssh_scp_request_get_filename(_scp)));
            
            ofstream ofs; ofs.open(path.c_str(), ios_base::binary);
            if (!ofs.is_open())
                throw runtime_error("destination dir is not available");
            
            // now prepared to get the input data
            ::ssh_scp_accept_request(_scp);

            int total_read = 0;
            do 
            {
                char buffer[16384] = { 0 };
                rc = ::ssh_scp_read(_scp, (void*)buffer, sizeof(buffer));
                if (rc == SSH_ERROR)
                {
                    _ssh->error("error.reading_scp:");

                    cout << _ssh->error_msg();
                    ::ssh_scp_close(_scp);
                    return false; 
                }

                if (rc > 0)
                {
                    ofs.write(buffer, rc);
                    total_read += rc;
                }
            } while (total_read < size);
        }
        else if (rc == SSH_SCP_REQUEST_NEWDIR)
        { 
            auto name = string(::ssh_scp_request_get_filename(_scp));
            auto mode = ::ssh_scp_request_get_permissions(_scp);
            dir_components.push_back(name);
            // create_path(to, dir_components);

            ::ssh_scp_accept_request(_scp);
        }
        else if (rc == SSH_SCP_REQUEST_ENDDIR)
        {
            dir_components.pop_back();
        }
        else if (rc == SSH_SCP_REQUEST_WARNING)
        {
            cout << ::ssh_scp_request_get_warning(_scp);
        }
        else if (rc == SSH_SCP_REQUEST_EOF)
        {
            cout << "end of file\n";
            break;
        }
    }
    while(true);
    
    return true;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace ssh
} // namespace comm
} // namespace utility
