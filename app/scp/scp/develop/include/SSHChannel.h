#ifndef SSHCHANNEL_H_SX8H4IT1
#define SSHCHANNEL_H_SX8H4IT1

#include <libssh/libssh.h>
#include <stdint.h>

#include <boost/function.hpp>

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHSession;

class SSHChannel
{
public:
    typedef boost::function<void (int64_t)> ProgressF;
    typedef boost::function<bool (void)> StopF;

public:
    SSHChannel(SSHSession* session);
    ~SSHChannel();

public:
    bool open();
    void close();
    void register_callback(ProgressF callback, StopF shouldstop);

    template <typename F>
    int64_t remote_exec(char const* cmd, F write_to)
    {
        auto rc = ::ssh_channel_request_exec(_channel, cmd);
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
            uint8_t  buffer[BUF_SIZE] = { 0 };

            nbytes = ::ssh_channel_read(_channel, buffer, sizeof(buffer), 0);
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

        ::ssh_channel_send_eof(_channel);

        return total_read;
    }

private:
    ProgressF _progress;
    StopF     _should_stop;

private:
    ssh_channel _channel;
    SSHSession* _ssh;

    bool _is_open;
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
