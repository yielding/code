#include "stdafx.h"
#include "SSHSession.h"

#include "SSHChannel.h"
#include "SCPChannel.h"

#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <cassert>

using namespace std;
using namespace boost;

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#define SSH_ERROR_RET(msg) do { \
   disconnect();      \
   error(msg);        \
   _has_error = true; \
   return false;      \
} while(0)

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHSession::SSHSession(string const& host, int port)
    : _connected(false)
{
    int verbosity = SSH_LOG_PROTOCOL;
    _session = ::ssh_new();
    if (_session == NULL)
        throw std::runtime_error("SSH session creation error");

    ::ssh_options_set(_session, SSH_OPTIONS_HOST, host.c_str());
    ::ssh_options_set(_session, SSH_OPTIONS_PORT, &port);
    ::ssh_options_set(_session, SSH_OPTIONS_LOG_VERBOSITY, &verbosity);

    _has_error = false;
}

SSHSession::~SSHSession()
{
    disconnect();

    ::ssh_free(_session);
    ::ssh_finalize();
}

void SSHSession::disconnect()
{
    if (!_connected) 
        return;

    _connected = false;
    ::ssh_disconnect(_session);
}
    
bool SSHSession::connect(string const& id, string const& passwd)
{
    int rc = ::ssh_connect(_session);
    if (rc != SSH_OK)
        SSH_ERROR_RET("session.connect:");

    if (!verify_known_host())
        SSH_ERROR_RET("session.verification:");

    //
    // try to authenticate through "none" method.
    // This seldom, if ever, happens.
    rc = ::ssh_userauth_none(_session, NULL);
    if (rc == SSH_AUTH_ERROR)
        return false;

    //
    // query available authentication list
    // one of [SSH_AUTH_METHOD_NONE,      SSH_AUTH_METHOD_PASSWORD,
    //         SSH_AUTH_METHOD_PUBLICKEY, SSH_AUTH_METHOD_HOSTBASED,
    //         SSH_AUTH_METHOD_INTERACTIVE]
    int method = ::ssh_userauth_list(_session, NULL);

    while (rc != SSH_AUTH_SUCCESS)
    {
        if (method & SSH_AUTH_METHOD_PUBLICKEY)
        {
            rc = ::ssh_userauth_autopubkey(_session, NULL);
            if (rc == SSH_AUTH_SUCCESS) break;
            if (rc == SSH_AUTH_ERROR)
                SSH_ERROR_RET("");
        }

        if (method & SSH_AUTH_METHOD_INTERACTIVE)
        {
            auto err = ::ssh_userauth_kbdint(_session, NULL, NULL);
            while (err == SSH_AUTH_INFO)
            {
                auto times = ::ssh_userauth_kbdint_getnprompts(_session);
                for (int i = 0; i < times; ++i)
                {
                    char echo;
                    auto prompt = ::ssh_userauth_kbdint_getprompt(_session, i, &echo);
                    if (prompt == nullptr)
                        break;

                    assert(echo == '\0');
                    if (ssh_userauth_kbdint_setanswer(_session, i, passwd.c_str()) < 0)
                        SSH_ERROR_RET("auto.error:");
                }

                err = ::ssh_userauth_kbdint(_session, NULL, NULL);
            }
            
            if (err == SSH_AUTH_DENIED)
                return false;

            break;
        }

        if (method & SSH_AUTH_METHOD_PASSWORD)
        {
            if (ssh_userauth_password(_session, NULL, passwd.c_str()) == 
                SSH_AUTH_SUCCESS)
                break;

            SSH_ERROR_RET("Error authenticating with password:");
        }
    }

    _connected = true;

    return true;
}

bool SSHSession::verify_known_host()
{
    //
    // REMARK
    // we can find more detailed information when state is not SSH_SERVER_KNOWN_OK;
    // SSH_SERVER_KNOWN_CHANGED, SSH_SERVER_FOUND_OTHER
    // SSH_SERVER_FILE_NOT_FOUND SSH_SERVER_NOT_KNOWN SSH_SERVER_ERROR
    //
    return ::ssh_is_server_known(_session) == SSH_SERVER_KNOWN_OK;
}

void SSHSession::error(string const& msg)
{
    _has_error = true;
    _error_msg = str(format("%s: %s") % msg % ::ssh_get_error(_session));
}

auto SSHSession::create_ssh_channel() -> boost::shared_ptr<SSHChannel>
{
    boost::shared_ptr<SSHChannel> channel(new SSHChannel(this));
    if (!channel->open())
        throw std::runtime_error("SSH channel creation error");

    return channel;
}

auto SSHSession::create_scp_channel() -> boost::shared_ptr<SCPChannel>
{
    boost::shared_ptr<SCPChannel> channel(new SCPChannel(this));

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
