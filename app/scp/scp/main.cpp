/**/
#include "SSHSession.h"

#include <iostream>
#include <boost/format.hpp>

using namespace std;
using namespace boost;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
    // utility::comm::ssh::SSHSession ssh("219.241.220.98");
    utility::comm::ssh::SSHSession ssh("127.0.0.1");

    if (!ssh.connect("yielding", "alsrudk!"))
    {
        cout << str(format("ssh.connect error: %s\n") % ssh.error_msg());
        return EXIT_FAILURE;
    }

    cout << "ssh.connect ok\n";

    return EXIT_SUCCESS;
}
 
/**/

/** /
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#include <libssh/libssh.h>

int authenticate_console(ssh_session session);
int verify_knownhost(ssh_session session);
ssh_session connect_ssh(const char *hostname, const char *user, int verbosity);

int authenticate_kbdint(ssh_session session, const char *password)
{
    int err;
    
    err = ssh_userauth_kbdint(session, NULL, NULL);
    while (err == SSH_AUTH_INFO)
    {
        const char *instruction;
        const char *name;
        char buffer[128];
        int i, n;
        
        name = ssh_userauth_kbdint_getname(session);
        instruction = ssh_userauth_kbdint_getinstruction(session);
        n = ssh_userauth_kbdint_getnprompts(session);
        
        if (name && strlen(name) > 0)
            printf("%s\n", name);
        
        if (instruction && strlen(instruction) > 0)
            printf("%s\n", instruction);
        
        for (i = 0; i < n; i++) {
            const char *answer;
            const char *prompt;
            char echo;
            
            prompt = ssh_userauth_kbdint_getprompt(session, i, &echo);
            if (prompt == NULL)
                break;
            
            if (echo) {
                char *p;
                
                printf("%s", prompt);
                
                if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
                    return SSH_AUTH_ERROR;
                }
                
                buffer[sizeof(buffer) - 1] = '\0';
                if ((p = strchr(buffer, '\n'))) {
                    *p = '\0';
                }
                
                if (ssh_userauth_kbdint_setanswer(session, i, buffer) < 0) {
                    return SSH_AUTH_ERROR;
                }
                
                memset(buffer, 0, strlen(buffer));
            } else {
                if (password && strstr(prompt, "Password:")) {
                    answer = password;
                } else {
                    buffer[0] = '\0';
                    
                    if (ssh_getpass(prompt, buffer, sizeof(buffer), 0, 0) < 0) {
                        return SSH_AUTH_ERROR;
                    }
                    answer = buffer;
                }
                if (ssh_userauth_kbdint_setanswer(session, i, answer) < 0) {
                    return SSH_AUTH_ERROR;
                }
            }
        }
        
        err=ssh_userauth_kbdint(session,NULL,NULL);
    }
    
    return err;
}

void error(ssh_session s)
{
    
}

int authenticate_console(ssh_session session)
{
    char password[128] = {0};
    char *banner;
    
    // 1. try to authenticate
    int rc = ssh_userauth_none(session, NULL);
    if (rc == SSH_AUTH_ERROR)
    {
        error(session);
        return rc;
    }
    
    if (rc == SSH_AUTH_SUCCESS)
    {
        // TODO
    }
    
    int method = ssh_auth_list(session);
    while (rc != SSH_AUTH_SUCCESS)
    {
        // Try to authenticate with public key first
        if (method & SSH_AUTH_METHOD_PUBLICKEY) {
            rc = ssh_userauth_autopubkey(session, NULL);
            if (rc == SSH_AUTH_ERROR) {
                error(session);
                return rc;
            } else if (rc == SSH_AUTH_SUCCESS) {
                break;
            }
        }
        
        // Try to authenticate with keyboard interactive";
        if (method & SSH_AUTH_METHOD_INTERACTIVE) {
            rc = authenticate_kbdint(session, "alsrudk!");
            if (rc == SSH_AUTH_ERROR) {
                error(session);
                return rc;
            } else if (rc == SSH_AUTH_SUCCESS) {
                break;
            }
        }
        
        if (ssh_getpass("Password: ", password, sizeof(password), 0, 0) < 0) {
            return SSH_AUTH_ERROR;
        }
        
        // Try to authenticate with password
        if (method & SSH_AUTH_METHOD_PASSWORD) {
            rc = ssh_userauth_password(session, NULL, password);
            if (rc == SSH_AUTH_ERROR) {
                error(session);
                return rc;
            } else if (rc == SSH_AUTH_SUCCESS) {
                break;
            }
        }
    }
    
    banner = ssh_get_issue_banner(session);
    if (banner) {
        printf("%s\n",banner);
        ssh_string_free_char(banner);
    }
    
    return rc;
}

int verify_knownhost(ssh_session session)
{
    char *hexa;
    int state;
    char buf[10];
    unsigned char *hash = NULL;
    int hlen;
    
    state=ssh_is_server_known(session);
    
    hlen = ssh_get_pubkey_hash(session, &hash);
    if (hlen < 0) {
        return -1;
    }
    switch(state){
        case SSH_SERVER_KNOWN_OK:
            break; 
        case SSH_SERVER_KNOWN_CHANGED:
            fprintf(stderr,"Host key for server changed : server's one is now :\n");
            ssh_print_hexa("Public key hash",hash, hlen);
            ssh_clean_pubkey_hash(&hash);
            fprintf(stderr,"For security reason, connection will be stopped\n");
            return -1;
        case SSH_SERVER_FOUND_OTHER:
            fprintf(stderr,"The host key for this server was not found but an other type of key exists.\n");
            fprintf(stderr,"An attacker might change the default server key to confuse your client"
                    "into thinking the key does not exist\n"
                    "We advise you to rerun the client with -d or -r for more safety.\n");
            return -1;
        case SSH_SERVER_FILE_NOT_FOUND:
            fprintf(stderr,"Could not find known host file. If you accept the host key here,\n");
            fprintf(stderr,"the file will be automatically created.\n");
            // fallback to SSH_SERVER_NOT_KNOWN behavior 
        case SSH_SERVER_NOT_KNOWN:
            hexa = ssh_get_hexa(hash, hlen);
            fprintf(stderr,"The server is unknown. Do you trust the host key ?\n");
            fprintf(stderr, "Public key hash: %s\n", hexa);
            ssh_string_free_char(hexa);
            if (fgets(buf, sizeof(buf), stdin) == NULL) {
                ssh_clean_pubkey_hash(&hash);
                return -1;
            }
            if(strncasecmp(buf,"yes",3)!=0){
                ssh_clean_pubkey_hash(&hash);
                return -1;
            }
            fprintf(stderr,"This new key will be written on disk for further usage. do you agree ?\n");
            if (fgets(buf, sizeof(buf), stdin) == NULL) {
                ssh_clean_pubkey_hash(&hash);
                return -1;
            }
            if(strncasecmp(buf,"yes",3)==0){
                if (ssh_write_knownhost(session) < 0) {
                    ssh_clean_pubkey_hash(&hash);
                    fprintf(stderr, "error %s\n", strerror(errno));
                    return -1;
                }
            }
            
            break;
        case SSH_SERVER_ERROR:
            ssh_clean_pubkey_hash(&hash);
            fprintf(stderr,"%s",ssh_get_error(session));
            return -1;
    }
    ssh_clean_pubkey_hash(&hash);
    return 0;
}

ssh_session connect_ssh(const char *host, const char *user,int verbosity){
    ssh_session session;
    int auth=0;
    
    session=ssh_new();
    if (session == NULL) {
        return NULL;
    }
    
    if(user != NULL){
        if (ssh_options_set(session, SSH_OPTIONS_USER, user) < 0) {
            ssh_free(session);
            return NULL;
        }
    }
    
    if (ssh_options_set(session, SSH_OPTIONS_HOST, host) < 0) {
        ssh_free(session);
        return NULL;
    }
    ssh_options_set(session, SSH_OPTIONS_LOG_VERBOSITY, &verbosity);
    if (ssh_connect(session))
    {
        fprintf(stderr,"Connection failed : %s\n",ssh_get_error(session));
        ssh_disconnect(session);
        ssh_free(session);
        return NULL;
    }

    if (verify_knownhost(session)<0)
    {
        ssh_disconnect(session);
        ssh_free(session);
        return NULL;
    }

    auth = authenticate_console(session);
    if (auth==SSH_AUTH_SUCCESS)
    {
        return session;
    } 
    else if(auth==SSH_AUTH_DENIED)
    {
        fprintf(stderr,"Authentication failed\n");
    } 
    else 
    {
        fprintf(stderr,"Error while authenticating : %s\n",ssh_get_error(session));
    }

    ssh_disconnect(session);
    ssh_free(session);
    return NULL;
}

int verbosity=0;
const char *createcommand="rm -fr /tmp/libssh_tests && mkdir /tmp/libssh_tests && cd /tmp/libssh_tests && date > a && date > b && mkdir c && date > d";
char *host ="127.0.0.1";

static void usage(const char *argv0){
    fprintf(stderr,"Usage : %s [options] host\n"
            "sample tiny scp downloader client - libssh-%s\n"
            "This program will create files in /tmp and try to fetch them\n",
            //      "Options :\n",
            //      "  -r : use RSA to verify host public key\n",
            argv0,
            ssh_version(0));
    exit(0);
}

static void create_files(ssh_session session){
	ssh_channel channel=ssh_channel_new(session);
	char buffer[1];
	if(channel == NULL){
		fprintf(stderr,"Error creating channel: %s\n",ssh_get_error(session));
		exit(EXIT_FAILURE);
	}
	if(ssh_channel_open_session(channel) != SSH_OK){
		fprintf(stderr,"Error creating channel: %s\n",ssh_get_error(session));
		ssh_channel_free(channel);
		exit(EXIT_FAILURE);
	}
	if(ssh_channel_request_exec(channel,createcommand) != SSH_OK){
		fprintf(stderr,"Error executing command: %s\n",ssh_get_error(session));
		ssh_channel_close(channel);
		ssh_channel_free(channel);
		exit(EXIT_FAILURE);
	}
	while(!ssh_channel_is_eof(channel)){
		ssh_channel_read(channel,buffer,1,1);
		if (write(1,buffer,1) < 0) {
            fprintf(stderr, "Error writing to buffer\n");
            ssh_channel_close(channel);
            ssh_channel_free(channel);
            return;
        }
	}
	ssh_channel_close(channel);
	ssh_channel_free(channel);
}

// TODO
static int fetch_files(ssh_session session)
{
    int size;
    char buffer[16384];
    int mode;
    char *filename;
    int r;
    ssh_scp scp=ssh_scp_new(session, SSH_SCP_READ | SSH_SCP_RECURSIVE, "/tmp/libssh_tests/ *");
    if(ssh_scp_init(scp) != SSH_OK){
        fprintf(stderr,"error initializing scp: %s\n",ssh_get_error(session));
        ssh_scp_free(scp);
        return -1;
    }
    printf("Trying to download 3 files (a,b,d) and 1 directory (c)\n");
    do {
        
        r=ssh_scp_pull_request(scp);
        switch(r){
            case SSH_SCP_REQUEST_NEWFILE:
                size=ssh_scp_request_get_size(scp);
                filename=strdup(ssh_scp_request_get_filename(scp));
                mode=ssh_scp_request_get_permissions(scp);
                printf("downloading file %s, size %d, perms 0%o\n",filename,size,mode);
                free(filename);
                ssh_scp_accept_request(scp);
                r=ssh_scp_read(scp,buffer,sizeof(buffer));
                if(r==SSH_ERROR){
                    fprintf(stderr,"Error reading scp: %s\n",ssh_get_error(session));
                    ssh_scp_close(scp);
                    ssh_scp_free(scp);
                    return -1;
                }
                printf("done\n");
                break;
            case SSH_ERROR:
                fprintf(stderr,"Error: %s\n",ssh_get_error(session));
                ssh_scp_close(scp);
                ssh_scp_free(scp);
                return -1;
            case SSH_SCP_REQUEST_WARNING:
                fprintf(stderr,"Warning: %s\n",ssh_scp_request_get_warning(scp));
                break;
            case SSH_SCP_REQUEST_NEWDIR:
                filename=strdup(ssh_scp_request_get_filename(scp));
                mode=ssh_scp_request_get_permissions(scp);
                printf("downloading directory %s, perms 0%o\n",filename,mode);
                free(filename);
                ssh_scp_accept_request(scp);
                break;
            case SSH_SCP_REQUEST_ENDDIR:
                printf("End of directory\n");
                break;
            case SSH_SCP_REQUEST_EOF:
                printf("End of requests\n");
                goto end;
        }
    } while (1);

end:
    ssh_scp_close(scp);
    ssh_scp_free(scp);
    return 0;
}

int main(int argc, char **argv){
    ssh_session session;
    // session=connect_ssh(host,NULL,SSH_LOG_PROTOCOL);
    session=connect_ssh(host,"yielding",SSH_LOG_PROTOCOL);

    if(session == NULL)
        return EXIT_FAILURE;

    create_files(session);
    fetch_files(session);
    ssh_disconnect(session);
    ssh_free(session);
    ssh_finalize();

    return 0;
}

/ **/
