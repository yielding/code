#ifndef SSHSESSION_H_W9K334XP
#define SSHSESSION_H_W9K334XP

#include <string>
#include <libssh/libssh.h>

#include <boost/shared_ptr.hpp>

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
    SSHSession(std::string const& host="127.0.0.1", int port=22);
    ~SSHSession();

    auto disconnect() -> void;
    auto connect(std::string const& id="root", std::string const& passwd="alpine") -> bool;
    
public:
    auto create_ssh_channel() -> boost::shared_ptr<SSHChannel>;
    auto create_scp_channel() -> boost::shared_ptr<SCPChannel>;

    auto error_msg() -> std::string { return _error_msg; }

protected:
    auto error(std::string const&) -> void;
    auto has_error() -> bool        { return _has_error; }

protected:
    auto verify_known_host() -> bool;

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
