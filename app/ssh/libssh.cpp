#include <libssh/libssh.h>

#include <cstdlib>
#include <iostream>

using namespace std;

char const* cmd = "dd if=/dev/zero bs=1024 count=1";

int remote_exec(ssh_session session)
{
    ssh_channel ch = ssh_channel_new(session);
    if (ch == NULL)
      return SSH_ERROR;

    int rc = ssh_channel_open_session(ch);
    if (rc != SSH_OK)
    {
      ssh_channel_free(ch);
      return rc;
    }

    rc = ssh_channel_request_exec(ch, cmd);
    if (rc != SSH_OK)
    {
      ssh_channel_close(ch);
      ssh_channel_free(ch);
      return rc;
    }

    int const BUF_SIZE = 1024 * 10;
    uint32_t buffer[BUF_SIZE] = { 0 };
    uint32_t nbytes = 0;
    do
    {
        nbytes = ssh_channel_read(ch, buffer, sizeof(buffer), 0);
        if (write(1, buffer, nbytes) != nbytes)
        {
          cout << "sjklfsjkfdjklfjdsklfjklsdfjkls\n";
          return SSH_ERROR;
        }

    } while(nbytes>0);

    if (nbytes < 0)
    {
      ssh_channel_close(ch);
      ssh_channel_free(ch);
      return SSH_ERROR;
    }

    ssh_channel_send_eof(ch);
    ssh_channel_close(ch);
    ssh_channel_free(ch);

    return 1;
}

int main(int argc, char const* argv[])
{
    ssh_session iphone_session;

    iphone_session = ssh_new();
    if (iphone_session == NULL)
        exit(EXIT_FAILURE);

    ssh_options_set(iphone_session, SSH_OPTIONS_HOST, "localhost");
    int port = 2222;
    ssh_options_set(iphone_session, SSH_OPTIONS_PORT, &port);

    int rc = ssh_connect(iphone_session);
    if (rc != SSH_OK)
    {
        cout << "Error connecting to localhost " 
             << ssh_get_error(iphone_session) << endl;
        ssh_free(iphone_session);
        exit(EXIT_FAILURE);
    }
    
    char const* passwd = "";
    rc = ssh_userauth_password(iphone_session, "root", passwd);
    if (rc != SSH_AUTH_SUCCESS)
    {
        fprintf(stderr, "Error authenticating with password: %s\n",
            ssh_get_error(iphone_session));

        ssh_disconnect(iphone_session);
        ssh_free(iphone_session);
        exit(-1);
    }

    remote_exec(iphone_session);

    ssh_disconnect(iphone_session);
    ssh_free(iphone_session);

    return 0;
}
