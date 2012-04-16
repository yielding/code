#ifndef SSHSESSION_H_W9K334XP
#define SSHSESSION_H_W9K334XP

#include <libssh/libssh.h>

#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/format.hpp>

#include <fstream>
#include <sstream>
#include <stdint.h>

using namespace std;
using namespace boost;

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHChannel
{
public:
    typedef boost::function<void (int64_t)> ProgressFunc;
    typedef boost::function<bool (void)> StopFunc;

public:
    SSHChannel(ssh_session session);

    ~SSHChannel();

    bool open();

    void close();

    void register_callback(ProgressFunc callback, StopFunc shouldstop);

    template <typename F>
    int64_t remote_exec(char const* cmd, F write_to)
    {
        auto rc = ssh_channel_request_exec(_channel, cmd);
        if (rc != SSH_OK)
            return 0;

        int32_t nbytes = 0;
        int64_t total_read = 0;
        int count = 0;
        do
        {
            if (!_should_stop.empty() && _should_stop())
                break;

            int const BUF_SIZE = 1024 * 10;
            uint8_t buffer[BUF_SIZE] = { 0 };

            nbytes = ssh_channel_read(_channel, buffer, sizeof(buffer), 0);
            if (nbytes > 0)
            {
                write_to(buffer, nbytes);
                total_read += nbytes;
            }

            if (++count == 100)
            {
                if (!_progress.empty())
                    _progress(total_read);

                count = 0;
            }

        } while (nbytes > 0);

        if (!_progress.empty())
            _progress(total_read);

        if (nbytes < 0)
            return total_read;

        ssh_channel_send_eof(_channel);

        return total_read;
    }

private:
    ProgressFunc _progress;
    StopFunc     _should_stop;

private:
    ssh_channel _channel;
    bool _is_open;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SCPChannel
{
};

/*
class SCPConnection
{
public:
    SCPConnection()
    {
    }

    bool connect()
    {
      _scp = ssh_scp_new(session, SSH_SCP_READ, file);
    }

    void read()
    {
    }

    void write()
    {
    }

    ssh_scp _scp;

    ssh_session _session;
};
*/

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHSession
{
public:
    SSHSession(char const* host="127.0.0.1", int port=2222);
    ~SSHSession();

    auto disconnect() -> void;
    auto connect(char const* id="root", char const* passwd="alpine") -> bool;
    auto has_error() -> bool   { return m_has_error; }
    auto error_msg() -> string { return m_error_msg; }
    auto create_channel() -> boost::shared_ptr<SSHChannel>;

private:
    string m_error_msg;
    bool   m_has_error;
    bool   m_connected;

private:
    ssh_session m_session;
};

} // end of ssh
} // end of comm
} // end of utility

#endif /* end of include guard: SSHSESSION_H_W9K334XP */
