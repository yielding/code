#include "stdafx.h"
#include "SSHChannel.h"
#include "SSHSession.h"

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHChannel::SSHChannel(SSHSession* session)
    : _is_open(false)
    , _ssh(session)
{
    _channel = ssh_channel_new(_ssh->session());
    if (_channel == NULL)
        throw std::runtime_error("SSH channel creation error");
}

SSHChannel::~SSHChannel()
{
    close();
    ::ssh_channel_free(_channel);
}

bool SSHChannel::open()
{
    _is_open = (::ssh_channel_open_session(_channel) == SSH_OK);
    return _is_open;
}

void SSHChannel::close()
{
    if (_is_open)
    {
        _is_open = false;
        ::ssh_channel_close(_channel);
    }
}

void SSHChannel::register_callback(ProgressF callback, StopF shouldstop)
{
    _progress    = callback;
    _should_stop = shouldstop;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace ssh
} // namespace comm
} // namespace utility
