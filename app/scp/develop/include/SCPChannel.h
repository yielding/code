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
        int rc = ::ssh_scp_pull_request(_scp);
        if (rc == SSH_SCP_REQUEST_NEWFILE)
        {
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
