#ifndef SSHSESSION_H_W9K334XP
#define SSHSESSION_H_W9K334XP

#include <libssh/libssh.h>

#include <memory>
#include <string>

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHChannel;
class SCPChannel;

class SSHSession
{
public:
    SSHSession(char const* host="127.0.0.1", int port=2222);
    ~SSHSession();

    auto disconnect() -> void;
    auto connect(char const* id="root", char const* passwd="alpine") -> bool;
    auto has_error()  -> bool        { return _has_error; }
    auto error_msg()  -> std::string { return _error_msg; }

public:
    auto create_ssh_channel() -> std::unique_ptr<SSHChannel>;
    auto create_scp_channel() -> std::unique_ptr<SCPChannel>;

private:
    std::string _error_msg;
    bool _has_error;
    bool _connected;

private:
    ssh_session _session;
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
