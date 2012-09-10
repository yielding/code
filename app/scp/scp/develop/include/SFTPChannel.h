#ifndef SFTPCHANNEL_H
#define SFTPCHANNEL_H

#include <libssh/libssh.h>
#include <libssh/sftp.h>

#include <boost/function.hpp>

#include <string>
#include <vector>

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using std::string;

class SSHSession;
 
class SFTPChannel
{
public:
    typedef boost::function<void(char* bufffer, size_t)>
      MDFWriter;
    
    typedef boost::function<void(string const&, uint64_t, bool, uint64_t, uint32_t, uint32_t)>
       Block;

public:
    SFTPChannel(SSHSession* session);
    ~SFTPChannel();
    
    bool scan_dir(string const& from, Block b);
    bool download_to(string const& from, MDFWriter);
    auto discarded_files() -> std::vector<std::string>& { return _discarded_files; }
    
    bool save_file_to(string const& fpath);
    
private:
    SSHSession* _ssh;
    std::vector<string> _discarded_files;
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
