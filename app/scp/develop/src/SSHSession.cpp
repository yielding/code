#include "SSHSession.h"

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHChannel::SSHChannel(ssh_session session)
    : _is_open(false)
{
    _channel = ssh_channel_new(session);
    if (_channel == NULL)
        std::runtime_error("SSH channel creation error");
}

SSHChannel::~SSHChannel()
{
    close();
    ssh_channel_free(_channel);
}

bool SSHChannel::open()
{
    _is_open = ssh_channel_open_session(_channel) == SSH_OK;
    return _is_open;
}

void SSHChannel::close()
{
    if (_is_open)
    {
        _is_open = false;
        ssh_channel_close(_channel);
    }
}

void SSHChannel::register_callback(ProgressFunc callback, StopFunc shouldstop)
{
    _progress = callback;
    _should_stop = shouldstop;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHSession::SSHSession(char const* host, int port)
    : m_connected(false)
{
    m_session = ssh_new();
    if (m_session == NULL)
        throw std::runtime_error("SSH session creation error");

    ssh_options_set(m_session, SSH_OPTIONS_HOST, host);
    ssh_options_set(m_session, SSH_OPTIONS_PORT, &port);

    m_has_error = false;
}

SSHSession::~SSHSession()
{
    disconnect();
    ssh_free(m_session);
}

void SSHSession::disconnect()
{
    if (!m_connected) 
        return;

    m_connected = false;
    ssh_disconnect(m_session);
}

bool SSHSession::connect(char const* id, char const* passwd)
{
    using namespace boost;

    int rc = ssh_connect(m_session);
    if (rc != SSH_OK)
    {
        m_has_error = true;
        m_error_msg = ssh_get_error(m_session);
        return false;
    }

    m_connected = true;

    rc = ssh_userauth_password(m_session, id, passwd);
    if (rc != SSH_AUTH_SUCCESS)
    {
        m_has_error = true;
        m_error_msg = boost::str(format("Error authenticating with password: %s")
            % ssh_get_error(m_session));

        disconnect();
        return false;
    }

    return true;
}

boost::shared_ptr<SSHChannel> SSHSession::create_channel()
{
    boost::shared_ptr<SSHChannel> 
        channel(new SSHChannel(m_session));

    if (!channel->open())
    {
        throw std::runtime_error("SSH channel creation error");
    }

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
