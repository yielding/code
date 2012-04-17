#include "SSHSession.h"

#include "SSHChannel.h"
#include "SCPChannel.h"

#include <boost/format.hpp>

using namespace boost;

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHSession::SSHSession(char const* host, int port)
    : _connected(false)
{
    _session = ::ssh_new();
    if (_session == NULL)
        throw std::runtime_error("SSH session creation error");

    ::ssh_options_set(_session, SSH_OPTIONS_HOST, host);
    ::ssh_options_set(_session, SSH_OPTIONS_PORT, &port);

    _has_error = false;
}

SSHSession::~SSHSession()
{
    disconnect();

    ::ssh_free(_session);
}

void SSHSession::disconnect()
{
    if (!_connected) 
        return;

    _connected = false;
    ::ssh_disconnect(_session);
}

bool SSHSession::connect(char const* id, char const* passwd)
{
    int rc = ssh_connect(_session);
    if (rc != SSH_OK)
    {
        _has_error = true;
        _error_msg = ssh_get_error(_session);
        return false;
    }

    _connected = true;

    rc = ssh_userauth_password(_session, id, passwd);
    if (rc != SSH_AUTH_SUCCESS)
    {
        _has_error = true;
        _error_msg = str(format("Error authenticating with password: %s")
            % ::ssh_get_error(_session));

        disconnect();
        return false;
    }

    return true;
}

auto SSHSession::create_ssh_channel() -> std::unique_ptr<SSHChannel>
{
    std::unique_ptr<SSHChannel> channel(new SSHChannel(_session));

    if (!channel->open())
        throw std::runtime_error("SSH channel creation error");

    return channel;
}

auto SSHSession::create_scp_channel() -> std::unique_ptr<SCPChannel>
{
    std::unique_ptr<SCPChannel> channel(new SCPChannel(_session));
    if (!channel->open())
        throw std::runtime_error("SCP channel creation error");

    return channel;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace ssh
} // namespace comm
} // namespace utility
