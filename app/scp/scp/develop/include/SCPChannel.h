#ifndef SCPCHANNEL_H_HOEYHUOE
#define SCPCHANNEL_H_HOEYHUOE

#include <string>
#include <boost/function.hpp>
#include <libssh/libssh.h>

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SCPChannel
{
public:
    SCPChannel(ssh_session session)
        : _is_open(false)
    {
        std::string path = ".";
        _scp = ::ssh_scp_new(session, SSH_SCP_READ | SSH_SCP_RECURSIVE, path.c_str());
        if (_scp == NULL)
            throw std::runtime_error("SCP channel creation error");
    }

    ~SCPChannel()
    {
        close();
        ::ssh_scp_free(_scp);
    }

    // REMARK: ask SSHSession what's happened when open fails
    bool open()
    {
        _is_open = (::ssh_scp_init(_scp) == SSH_OK);
        return _is_open;
    }

    void close()
    {
        if (_is_open)
        {
            _is_open = false;
            ::ssh_scp_close(_scp);
        }
    }

    void read()
    {
        do 
        {
            int rc = ::ssh_scp_pull_request(_scp);
            if (rc == SSH_SCP_REQUEST_NEWFILE)
            {
                char buffer[16384] = { 0 };
                auto size = ::ssh_scp_request_get_size(_scp);
                auto name = string(::ssh_scp_request_get_filename(_scp));
                auto mode = ::ssh_scp_request_permissions(_scp);

                ::ssh_scp_accept_request(_scp);
                rc = ::ssh_scp_read(_scp, buffer, sizeof(buffer));
            }

            if (rc == SSH_SCP_REQUEST_NEWDIR)
            {
            }

            if (rc == SSH_SCP_REQUEST_ENDDIR)
            {
            }

            if (rc == SSH_SCP_REQUEST_EOF)
            {}
        }
        while (true);
    }

private:
    bool _is_open;
    ssh_scp _scp;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace ssh
} // namespace comm
} // namespace utility

#endif
