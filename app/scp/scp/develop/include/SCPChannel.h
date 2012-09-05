#ifndef SCPCHANNEL_H_HOEYHUOE
#define SCPCHANNEL_H_HOEYHUOE

#include <libssh/libssh.h>

#include <string>
#include <vector>

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHSession;
 
/*
class SCPChannel
{
public:
    SCPChannel(SSHSession* session);
    ~SCPChannel();

    bool download(std::string const& from, std::string const& to);
    bool save_file(std::string fullpath);
    
private:
    ssh_scp _scp;
    SSHSession* _ssh;
};
*/
  
    class SCPChannel
    {
    public:
        SCPChannel(SSHSession* session);
        ~SCPChannel();
        
        bool download(std::string const& from, std::string const& to);
        std::vector<std::string>& discarded_files() { return _discarded_files; }
        
        bool save_file(std::string fpath);
        
    private:
        std::vector<std::string> _discarded_files;
        ssh_scp _scp;
        SSHSession* _ssh;
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