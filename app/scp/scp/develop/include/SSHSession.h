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
class SFTPChannel;

using std::string;

class SSHSession
{
public:
    SSHSession(string const& host, string const& user, int port=22);
    ~SSHSession();

    auto disconnect() -> void;
    auto connect(string const& passwd) -> bool;
    
    auto session() -> ssh_session&  { return _session;    }

public:
    auto create_ssh_channel()  -> boost::shared_ptr<SSHChannel>;
    auto create_scp_channel()  -> boost::shared_ptr<SCPChannel>;
    auto create_sftp_channel() -> boost::shared_ptr<SFTPChannel>;

public:
    auto error_msg() -> string { return _error_msg; }
    auto error(string const&) -> void;
    auto has_error() -> bool   { return _has_error; }

protected:
    auto verify_known_host() -> bool;

private:
    string _error_msg;
    bool   _has_error;
    bool   _connected;

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
