#ifndef SCPCHANNEL_H_FSVO3QO8
#define SCPCHANNEL_H_FSVO3QO8

#include "SCPChannel.h"
#include "SSHSession.h"
#include "scope_guard.h"

#include <string>
#include <stdexcept>
#include <boost/phoenix.hpp>

using namespace std;

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
    ::ssh_scp_free(_scp);
}

bool SCPChannel::download(string const& from, string const& to)
{
    namespace phx = boost::phoenix;

    string path = from + " " + to;
    
    _scp = ::ssh_scp_new(_ssh->session(), 
                         SSH_SCP_READ | SSH_SCP_RECURSIVE, path.c_str());
    if (_scp == NULL)
        throw std::runtime_error("SCP channel creation error");
    
    ON_BLOCK_EXIT(phx::bind(&ssh_scp_free, phx::ref(_scp)));
    
    if (::ssh_scp_init(_scp) != SSH_OK)
    {
        _ssh->error("scp.download: ");
        return false;
    }
    
    do 
    {
        int rc = ::ssh_scp_pull_request(_scp);
        if (rc == SSH_SCP_REQUEST_NEWFILE)
        {
            char buffer[16384] = { 0 };
            auto size = ::ssh_scp_request_get_size(_scp);
            auto name = string(::ssh_scp_request_get_filename(_scp));
            auto mode = ::ssh_scp_request_get_permissions(_scp);

            ::ssh_scp_accept_request(_scp);
            // 여기서 돌아야 할 듯.
            rc = ::ssh_scp_read(_scp, buffer, sizeof(buffer));
            if (rc == SSH_ERROR)
            {
                _ssh->error("error.reading_scp:");
                ::ssh_scp_close(_scp);
                return false; 
            }

            cout << "read done\n";
        }

        if (rc == SSH_ERROR)
        {
            cout << "error\n";
            ::ssh_scp_close(_scp);
            return false;
        }
        
        if (rc == SSH_SCP_REQUEST_WARNING)
        {
            cout << ::ssh_scp_request_get_warning(_scp);
        }

        if (rc == SSH_SCP_REQUEST_NEWDIR)
        { 
            auto name = string(::ssh_scp_request_get_filename(_scp));
            auto mode = ::ssh_scp_request_get_permissions(_scp);

            printf("downloading directory %s, perms 0%o\n", name.c_str(), mode);
            ::ssh_scp_accept_request(_scp);
        }

        if (rc == SSH_SCP_REQUEST_ENDDIR)
        {
            cout << "end of directory\n";
        }

        if (rc == SSH_SCP_REQUEST_EOF)
        {
            cout << "end of requrest\n";
            break;
        }
    }
    while (true);
    
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

#endif
